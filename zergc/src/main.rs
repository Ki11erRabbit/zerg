use clap::Parser;
use std::fs;
use std::path::{Path, PathBuf};
use parser::parse;
use ast::parse_tree::{File as AstFile, TopLevelStatement};
use ast::ResolverError;

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
}

fn main() {
    let args = Args::parse();
    let main_path = args.main_file;
    let main_dir = main_path.parent().unwrap_or_else(|| Path::new("."));

    // Store file contents to keep lifetimes
    let mut file_contents: Vec<(PathBuf, String)> = Vec::new();
    let mut errors: Vec<CollectError> = Vec::new();
    if let Err(e) = collect_files_by_ref(&main_path, main_dir, &mut file_contents, &mut errors) {
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
    match result {
        Ok(_) => (),
        Err(ResolverError::Many(errors)) => {
            for e in errors {
                eprintln!("Error: {}", e);
            }
        }
        Err(e) => {
            eprintln!("Error: {}", e);
        }
    }
}

/// Iteratively collect file paths and contents using explicit stack, storing (PathBuf, String)
fn collect_files_by_ref(
    main_file: &Path,
    main_dir: &Path,
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
        // Parse file for imports
        match parse(&content) {
            Ok(ast) => {
                for stmt in &ast.top_level_statements {
                    let import_path_opt = match stmt {
                        TopLevelStatement::Import(import_path) => Some(import_path),
                        TopLevelStatement::ComptimeImport(import_path) => Some(import_path),
                        _ => None,
                    };
                    if let Some(import_path) = import_path_opt {
                        let import_vec = import_path.to_vec_strings();
                        let mut dir = base_dir.clone();
                        for (i, seg) in import_vec.iter().enumerate() {
                            if i != import_vec.len() - 1 {
                                dir.push(seg);
                            } else {
                                dir.push(format!("{seg}.zerg", ));
                            }
                        }
                        if dir.exists() {
                            let path = dir.clone();
                            dir.pop();
                            stack.push((path, dir))
                        }
                    }
                }
            }
            Err(_) => {}
        }
    }
    Ok(())
}



