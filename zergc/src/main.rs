use clap::Parser;
use std::fs;
use std::path::{Path, PathBuf};
use parser::parse;
use ast::parse_tree::{File as AstFile, TopLevelStatement};
use ast::ResolverError;
use backend::Compiler;

#[derive(Debug)]
enum CollectError {
    Io(PathBuf, std::io::Error),
    Parse(PathBuf, String),
}

/// Command line arguments
#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
    /// Path to main.zerg file
    #[arg()]
    main_file: PathBuf,
    /// Optional directory containing the standard library sources
    #[arg(short = 's', long = "stdlib")]
    stdlib: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    let main_path = args.main_file;
    let main_dir = main_path.parent().unwrap_or_else(|| Path::new("."));

    // Store file contents to keep lifetimes
    let mut file_contents: Vec<(PathBuf, String)> = Vec::new();
    let mut errors: Vec<CollectError> = Vec::new();
    let stdlib_dir = args.stdlib.as_deref();
    if let Err(e) = collect_files_by_ref(&main_path, main_dir, stdlib_dir, &mut file_contents, &mut errors) {
        eprintln!("Error collecting files: {e:?}");
    }

    let mut files: Vec<(PathBuf, AstFile)> = Vec::new();
    for (path, content) in &file_contents {
        match parse(content) {
            Ok(ast) => files.push((path.clone(), ast)),
            Err(e) => {
                let err = CollectError::Parse(path.clone(), format!("{:?}", e));
                errors.push(err);
            }
        }
    }

    println!("Collected {} files:", files.len());
    for (path, _) in &files {
        println!("- {}", path.display());
    }
    if !errors.is_empty() {
        eprintln!("Encountered errors:");
        for err in errors {
            match err {
                CollectError::Io(path, io_err) => eprintln!("IO error reading {}: {}", path.display(), io_err),
                CollectError::Parse(path, msg) => eprintln!("Parse error in {}: {}", path.display(), msg),
            }
        }
    }

    let result = ast::desugar_and_typecheck(files);
    let files = match result {
        Ok(files) => files,
        Err(ResolverError::Many(errors)) => {
            for e in errors {
                eprintln!("Error: {}", e);
            }
            return;
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            return;
        }
    };

    let mut compiler = Compiler::new();
    compiler.compile_files(files).unwrap();
}

/// Iteratively collect file paths and contents using explicit stack, storing (PathBuf, String)
fn collect_files_by_ref(
    main_file: &Path,
    main_dir: &Path,
    stdlib_dir: Option<&Path>,
    file_contents: &mut Vec<(PathBuf, String)>,
    errors: &mut Vec<CollectError>,
) -> Result<(), ()> {
    use std::collections::HashSet;
    let mut stack = vec![(main_file.to_path_buf(), main_dir.to_path_buf())];
    let mut seen = HashSet::new();
    while let Some((file_path, base_dir)) = stack.pop() {
        if !seen.insert(file_path.clone()) {
            continue;
        }
        // Read file
        let content = match fs::read_to_string(&file_path) {
            Ok(s) => s,
            Err(e) => {
                let err = CollectError::Io(file_path.clone(), e);
                errors.push(err);
                return Err(());
            }
        };
        file_contents.push((file_path.clone(), content.clone()));
        // parse file for imports and add only the necessary ones
        if let Ok(ast) = parse(&content) {
            for stmt in &ast.top_level_statements {
                let import_path_opt = match stmt {
                    TopLevelStatement::Import(import_path) => Some(import_path),
                    TopLevelStatement::ComptimeImport(import_path) => Some(import_path),
                    _ => None,
                };
                if let Some(import_path) = import_path_opt {
                    let import_vec = import_path.to_vec_strings();

                    // helper to push a resolved path onto the stack
                    let mut push_candidate = |candidate: PathBuf| {
                        if candidate.exists() {
                            let mut dir = candidate.clone();
                            dir.pop();
                            stack.push((candidate, dir));
                        }
                    };

                    // try project-relative resolution first
                    let mut project_candidate = base_dir.to_path_buf();
                    for seg in &import_vec[..import_vec.len().saturating_sub(1)] {
                        project_candidate.push(seg);
                    }
                    if let Some(last) = import_vec.last() {
                        project_candidate.push(format!("{}.zerg", last));
                    }
                    if project_candidate.exists() {
                        push_candidate(project_candidate);
                    } else if let Some(stdlib_base) = stdlib_dir {
                        // if not found in project, try standard library directory
                        let mut std_candidate = stdlib_base.to_path_buf();
                        for seg in &import_vec[..import_vec.len().saturating_sub(1)] {
                            std_candidate.push(seg);
                        }
                        if let Some(last) = import_vec.last() {
                            std_candidate.push(format!("{}.zerg", last));
                        }
                        push_candidate(std_candidate);
                    }
                }
            }
        }
    }
    Ok(())
}



