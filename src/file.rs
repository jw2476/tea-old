use std::path::{Path, PathBuf};

pub struct File {
    pub content: String,
    pub path: PathBuf,
}

pub struct FileRegistry {
    files: Vec<File>,
}

impl FileRegistry {
    pub fn new() -> Self {
        Self { files: Vec::new() }
    }

    pub fn open<P: AsRef<Path>>(&mut self, path: P) -> anyhow::Result<&File> {
        let file = std::fs::read(path.as_ref())?;
        let content = String::from_utf8(file)?;

        let file = File {
            content,
            path: path.as_ref().to_path_buf(),
        };

        self.files.push(file);

        Ok(self.files.last().unwrap())
    }
}
