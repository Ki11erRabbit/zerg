use std::collections::{HashMap, HashSet};

pub struct FileDefinitions {
    /// The full file path including the name of the function
    exports: HashSet<Vec<String>>,
    /// The full file path including the name of the function
    internals: HashSet<Vec<String>>,
}

impl FileDefinitions {
    pub fn new() -> Self {
        Self {
            exports: HashSet::new(),
            internals: HashSet::new(),
        }
    }

    pub fn insert_export(&mut self, path: Vec<String>) {
        self.exports.insert(path);
    }

    pub fn insert_internal(&mut self, path: Vec<String>) {
        self.internals.insert(path);
    }

    pub fn lookup_export(&self, path: &[String]) -> bool {
        self.exports.contains(path)
    }

    pub fn lookup_internal(&self, path: &[String]) -> bool {
        self.internals.contains(path)
    }
}

pub struct FunctionResolver {
    /// The path includes just the file
    files: HashMap<Vec<String>, FileDefinitions>,
    /// The path includes just the files that are currently in scope
    current_paths: HashSet<Vec<String>>,
    /// The path includes just the files that are currently in scope
    current_comptime_paths: HashSet<Vec<String>>,
    current_module: Vec<String>,
}

impl FunctionResolver {
    pub fn new() -> Self {
        Self {
            files: HashMap::new(),
            current_paths: HashSet::new(),
            current_comptime_paths: HashSet::new(),
            current_module: Vec::new(),
        }
    }

    fn add_current_path(&mut self, path: Vec<String>) {
        self.current_paths.insert(path);
    }

    fn add_current_comptime_path(&mut self, path: Vec<String>) {
        self.current_comptime_paths.insert(path);
    }

    fn set_current_module(&mut self, module: Vec<String>) {
        self.current_module = module;
    }

    fn clear_paths(&mut self) {
        self.current_paths.clear();
        self.current_comptime_paths.clear();
        self.current_module.clear();
    }

    fn generate_paths(&self, item: &str) -> Vec<Vec<String>> {
        self.current_paths.iter().map(|path| {
            let mut path = path.clone();
            path.push(item.to_string());
            path
        }).collect()
    }

    fn generate_comptime_paths(&self, item: &str) -> Vec<Vec<String>> {
        self.current_comptime_paths.iter().map(|path| {
            let mut path = path.clone();
            path.push(item.to_string());
            path
        }).collect()
    }

    fn resolve_item(&self, item: &str) -> Option<Vec<String>> {
        let paths = self.generate_paths(item);

        for path in &paths {
            if let Some(def) = self.files.get(&path[..(path.len() - 1)]) {
                if def.lookup_export(path) {
                    return Some(path.clone());
                }
            }
        }

        let mut local_path = self.current_module.clone();
        local_path.push(item.to_string());
        if let Some(def) = self.files.get(&local_path[..(local_path.len() - 1)]) {
            if def.lookup_internal(&local_path) {
                return Some(local_path);
            }
        }

        None
    }

    fn resolve_comptime_item(&self, item: &str) -> Option<Vec<String>> {
        let paths = self.generate_comptime_paths(item);

        for path in &paths {
            if let Some(def) = self.files.get(&path[..(path.len() - 1)]) {
                if def.lookup_export(path) {
                    return Some(path.clone());
                }
            }
        }

        let mut local_path = self.current_module.clone();
        local_path.push(item.to_string());
        if let Some(def) = self.files.get(&local_path[..(local_path.len() - 1)]) {
            if def.lookup_internal(&local_path) {
                return Some(local_path);
            }
        }

        None
    }
}