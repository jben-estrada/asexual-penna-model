#!/usr/bin/python3
import argparse
from dis import COMPILER_FLAG_NAMES
import json
import os

# Script directory
SCRIPT_DIR = os.path.dirname(__file__)

# Output executable name
OUT = "penna.out"

# Compiler
FC = "gfortran-10"
# Compiler type
CMPTYPE = "gnu"

# Directory
SRC_DIR = f"{SCRIPT_DIR}/src"
LIB_DIR = f"{SCRIPT_DIR}/lib"
MOD_DIR = f"{SCRIPT_DIR}/mod"
OBJ_DIR = f"{SCRIPT_DIR}/obj"
BIN_DIR = f"{SCRIPT_DIR}/bin"

# External lib directory
EXTERNAL_DIR = f"{SCRIPT_DIR}/external"
STATIC_DIR = f"{SCRIPT_DIR}/static"
EXT_COMPILE_SETTING_FILE = "compile.json"

# Compiler flags
# NOTE: The intial apostrophe is required. The final one will be supplied by
#       `bld_spec_flag`
FCFLAGS = [
    "\"", "-c", "-std=f2008", "-Wall", "-Wextra", "-fcheck=all",
    "-march=native", "-pedantic"
]

# FoBiS.py command
FOBIS = "FoBiS.py"
# FoBiS.py build options: directory
DIRFLAGS = [
    "-s", SRC_DIR, LIB_DIR,
    "-dobj", OBJ_DIR,
    "-dmod", MOD_DIR
]
EXT_OBJDIRFLAGS = [
    "-dobj", OBJ_DIR,
    "-dmod", MOD_DIR
]
# FoBiS.py build flags
BLDFLAG = [
    "-fc", FC,
    "-compiler", CMPTYPE,
    "-o", f"{BIN_DIR}/{OUT}",
    *DIRFLAGS,
    "-colors",
    "-j", "2",
    "-q",
    "-cflags", *FCFLAGS
]
EXT_BLDFLAG= [
    "-fc", FC,
    "-compiler", CMPTYPE,
    "-colors",
    "-j", "2",
    "-q",
    "-cflags", *FCFLAGS
]

BUILD_TYPE = ["release", "debug", "static", "no-build"]


def parse_cmd_args():
    arg_parser = argparse.ArgumentParser(
        description="Build script for the asexual Penna model simulation code"
    )
    arg_parser.add_argument(
        "-t", "--type", choices=BUILD_TYPE, default="no-build",
        help="build type"
    )
    arg_parser.add_argument(
        "--clean", choices=["object", "mod", "none"], nargs="?",
        const="object", default="object", help="delete build files"
    )
    return arg_parser.parse_args()


def build_ext_lib(build_spec_flags):
    print("[Building external library]")
    static_lib_path_list = []
    for ext_lib in os.listdir(EXTERNAL_DIR):
        ext_lib_dir = f"{EXTERNAL_DIR}/{ext_lib}"
        if not os.path.isdir(ext_lib_dir):
            continue

        print(f"\n> Compiling {ext_lib}...")

        compile_setting_path = f"{ext_lib_dir}/{EXT_COMPILE_SETTING_FILE}"
        try:
            with open(compile_setting_path, "r") as file:
                data = json.load(file)
                target_file = f"{ext_lib_dir}/{data['target']}"
                cflag = data["cflag"]
        except FileNotFoundError as e:
            raise Exception(
                f"Compile setting file for \"{ext_lib_dir}\" not found"
            ) from e
        
        output_path = f"{STATIC_DIR}/{ext_lib}.a"
        build_flags = " ".join(
            [
                "-mklib", "static",
                "-t", target_file,
                "-o", output_path,
                "-s", ext_lib_dir,
                *EXT_OBJDIRFLAGS,
                *EXT_BLDFLAG, cflag, *build_spec_flags
            ]
        )

        os.system(f"{FOBIS} build {build_flags}")
        static_lib_path_list.append(output_path)
    return static_lib_path_list


def build():
    pass


def clean(type):
    if type == "object":
        print(f"\n[Removing {OBJ_DIR}/*.o files...]")
        os.system(f"rm -f {OBJ_DIR}/*.o")
        print(f"[Removing {STATIC_DIR}/*.a files...]")
        os.system(f"rm -f {STATIC_DIR}/*.a")
    elif type == "mod":
        print(f"\n[Removing {MOD_DIR}/*.(s)mod files...]")
        os.system(f"rm -f {MOD_DIR}/*.mod {MOD_DIR}/*.smod")


def main():
    build_args = parse_cmd_args()

    if build_args.type == "no-build" and build_args.clean == "none":
        raise ValueError(
            f"Choose something! Run: `{__file__} --help` for more information"
        )

    bld_spec_flag = ["\""]
    if build_args.type == "release":
        bld_spec_flag = ["-Ofast", "-g"] + bld_spec_flag
    elif build_args.type == "debug":
        bld_spec_flag = ["-O0", "-g"] + bld_spec_flag
    elif build_args.type == "static":
        bld_spec_flag = ["-Ofast", "-g", "-static"] + bld_spec_flag

    if build_args.type != "no-build":
        # Build the external libraries
        lib_path_list = build_ext_lib(bld_spec_flag)
        if lib_path_list:
            lib_path_list = ["-libs"] + lib_path_list

        # Build the entire project
        fobis_build_flags = " ".join(
            lib_path_list + BLDFLAG + bld_spec_flag
        )
        os.system(f"{FOBIS} build {fobis_build_flags}")

    if build_args.clean != "none":
        clean(build_args.clean)


if __name__ == "__main__":
    main()
    # build_ext_lib(["\""])
    # clean("object")