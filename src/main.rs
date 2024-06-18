mod compiler;
mod exporter;
mod parser;
mod util;

use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::{env, fs};

use compiler::compile;
use exporter::export;
use parser::parse;
use zip::ZipWriter;
use zip_extensions::ZipWriterExtensions;

fn main() -> anyhow::Result<()> {
    let mut args = env::args();
    let Some(_current_path) = args.next() else { panic!("Cannot get current dir") };
    let Some(in_path) = args.next() else { panic!("No input file given") };
    let Some(out_path) = args.next() else { panic!("No output path given") };

    let in_path = Path::new(&in_path);
    assert!(
        in_path.extension().and_then(|x| x.to_str()) == Some("ascm"),
        "input file must be a .ascm file"
    );

    let mut in_file = File::open(in_path).expect("input file should open");
    let input = {
        let mut buf = String::new();
        in_file.read_to_string(&mut buf).expect("should be able to read input file");
        buf
    };

    let parsed = parse(input)?;
    let compiled = compile(parsed)?;
    let exported = export(compiled).expect("export conversion should succeed");

    let in_filename = in_path.file_name().and_then(|name| name.to_str());
    let out_name = in_filename.map(|name| name.split('.')).and_then(|mut parts| parts.next());
    let out_name = out_name.expect("input path should have a filename prefix");

    let out_folder = Path::new(&out_path).join(out_name);

    let _ = fs::DirBuilder::new().create(&out_folder);
    let out_path_metadata = fs::metadata(&out_folder).expect("out path should exist");
    assert!(out_path_metadata.is_dir(), "out path should be a dir");

    let mut project_file = File::create(out_folder.join("project.json"))
        .expect("should be able to create project file");

    project_file.write_all(exported.as_bytes()).expect("should be able to write to out file");

    let res_path = Path::new("./res");

    fs_extra::dir::copy(
        res_path,
        &out_folder,
        &fs_extra::dir::CopyOptions::new().content_only(true),
    )
    .expect("res copy should succeed");

    let out_zip_path = Path::new(&out_path).join(format!("{}.sb3", out_name));
    let out_zip_file = File::create(out_zip_path).expect("should be able to create out zip file");
    let zip_writer = ZipWriter::new(out_zip_file);
    zip_writer.create_from_directory(&out_folder).expect("creating zip should succeed");

    let _ = fs::remove_dir_all(&out_folder);

    Ok(())
}
