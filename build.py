#!/usr/bin/python3
import argparse
import json
import os

from typing import NamedTuple
from enum import Enum

# Script directory
SCRIPT_DIR = os.path.dirname(__file__)

# Output executable name
OUTPUT_NAME = "penna"

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


BUILD_TYPES = ["release", "debug", "static", "no-build"]


class CompilerType(Enum):
    GNU   = "gnu"
    INTEL = "intel"


class BuildType(Enum):
    RELEASE = "release"
    DEBUG   = "debug"
    STATIC  = "static"
    NOBUILD = "no-build"


class CompilerInfo(NamedTuple):
    type:              CompilerType
    compiler:          str         # Compiler executable
    cflags:            list[str]   # Compile flags
    lflags:            list[str]   # Linking flags
    build_spec_cflags: dict[BuildType, list[str]]   # Build-specific cflags
    build_spec_lflags: dict[BuildType, list[str]]   # Build-specific lflags



COMPILER_GNU = CompilerInfo(
        type=CompilerType.GNU,
        compiler="gfortran",
        cflags=[
            "-c", "-std=f2018", "-Wall", "-Wextra", "-pedantic", "-march=native"
        ],
        lflags=[],
        build_spec_cflags={
            BuildType.RELEASE : ["-O3", "-g1"],
            BuildType.DEBUG   : ["-O0", "-g2", "-fcheck=all",
                                 "-fsanitize=address", "-static-libasan"],
            BuildType.STATIC  : ["-O3", "-g1", "-static"],
            BuildType.NOBUILD : []
        },
        build_spec_lflags={
            BuildType.RELEASE : [],
            BuildType.DEBUG   : ["-O0", "-fsanitize=address",
                                 "-static-libasan"],
            BuildType.STATIC  : [],
            BuildType.NOBUILD : []
        }
    )

COMPILER_INTEL = CompilerInfo(
        type=CompilerType.INTEL,
        compiler="ifx",
        cflags=[
            "-c", "-stand f18", "-xHost", "-warn all,stderrors,usage,unused"
        ],
        lflags=[],
        build_spec_cflags={
            BuildType.RELEASE : ["-O3", "-g1"],
            BuildType.DEBUG   : ["-O0", "-fsanitize address",
                                 "-fstack-protector-all", "-check all",
                                 "-debug all", "-g2", "-traceback"],
            BuildType.STATIC  : ["-O3", "-g1", "-static"],
            BuildType.NOBUILD : []
        },
        build_spec_lflags={
            BuildType.RELEASE : [],
            BuildType.DEBUG   : ["-O0", "-fsanitize address", "-check all"],
            BuildType.STATIC  : [],
            BuildType.NOBUILD : []
        }
    )

COMPILER_TYPE_DICT = {
    CompilerType.GNU:   COMPILER_GNU,
    CompilerType.INTEL: COMPILER_INTEL
}


# === FoBiS.py flags === #
# FoBiS.py executable
FOBIS = "FoBiS.py"
# Directory options
FOBIS_DIR_FLAGS = [
    "-s",   SRC_DIR, LIB_DIR,
    "-dobj", OBJ_DIR,
    "-dmod", MOD_DIR
]
# Directory options for external src code
FOBIS_EXT_DIR_FLAGS = [
    "-dobj", OBJ_DIR,
    "-dmod", MOD_DIR
]


def get_fobis_build_flags(
            cmp_type: CompilerType,
            build_type: BuildType,
            flag_type: str = "build",
            custom_cflags: list[str] | None = None
        ) -> list[str]:
    
    if cmp_type not in COMPILER_TYPE_DICT:
        raise ValueError(f"Invalid CompilerType: \"{repr(cmp_type)}\"")
    cmp_info = COMPILER_TYPE_DICT[cmp_type]
    
    if flag_type == "build":
        add_fobis_flags = [
            "-o", f"{BIN_DIR}/{OUTPUT_NAME}",
            *FOBIS_DIR_FLAGS,
            "-lflags \"",
                *cmp_info.lflags,
                *cmp_info.build_spec_lflags[build_type],
                "\""
        ]
    elif flag_type == "external-lib":
        add_fobis_flags = []   # Add new stuff if necessary
    else:
        add_fobis_flags = []

    return [
        "-fc",       cmp_info.compiler,
        "-compiler", cmp_info.type.value,
        *add_fobis_flags,
        "-colors",
        "-j", "2",
        "-q",
        "-cflags \"",
            *cmp_info.cflags,
            *cmp_info.build_spec_cflags[build_type],
            *(custom_cflags if custom_cflags else []),
            "\""
    ]


def build_external_lib(cmp_type: CompilerType, build_type: BuildType):
    print("[Building external library]")
    static_lib_path_list = []

    for ext_lib in os.listdir(EXTERNAL_DIR):
        ext_lib_dir = f"{EXTERNAL_DIR}/{ext_lib}"
        if not os.path.isdir(ext_lib_dir):
            continue

        print(f"\t> Compiling {ext_lib}...")
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
        ext_build_flags = get_fobis_build_flags(
                                cmp_type,
                                build_type,
                                flag_type="external-lib",
                                custom_cflags=[cflag]
                            )
        ext_build_flag_str = " ".join(
            [
                "-mklib", "static",
                "-t", target_file,
                "-o", output_path,
                "-s", ext_lib_dir,
                *FOBIS_EXT_DIR_FLAGS,
                *ext_build_flags
            ]
        )

        os.system(f"{FOBIS} build {ext_build_flag_str}")
        static_lib_path_list.append(output_path)
    return static_lib_path_list


def parse_cmd_args():
    arg_parser = argparse.ArgumentParser(
        description="Build script for the asexual Penna model simulation code"
    )
    arg_parser.add_argument(
        "-b", "--build-type",
        choices=list(map(lambda e: e.value, BuildType)),
        default=BuildType.NOBUILD.value, help="build type"
    )
    arg_parser.add_argument(
        "-c", "--compiler-type",
        choices=list(map(lambda e: e.value, CompilerType)),
        default=CompilerType.GNU.value, help="compiler type"
    )
    arg_parser.add_argument(
        "--clean", choices=["object", "mod", "all", "none"], nargs="?",
        const="object", default="object", help="delete build files"
    )
    return arg_parser.parse_args()


def clean(clean_type: str):
    print("")
    if clean_type == "object" or clean_type == "all":
        print(f"[Removing {OBJ_DIR}/*.o files...]")
        os.system(f"rm -f {OBJ_DIR}/*.o")
        print(f"[Removing {STATIC_DIR}/*.a files...]")
        os.system(f"rm -f {STATIC_DIR}/*.a")

    if clean_type == "mod"  or clean_type == "all":
        print(f"[Removing {MOD_DIR}/*.(s)mod files...]")
        os.system(f"rm -f {MOD_DIR}/*.mod {MOD_DIR}/*.smod")


def main():
    # Ensure that the destination directories exist
    os.makedirs(MOD_DIR, exist_ok=True)
    os.makedirs(BIN_DIR, exist_ok=True)
    os.makedirs(OBJ_DIR, exist_ok=True)
    os.makedirs(STATIC_DIR, exist_ok=True)

    build_args = parse_cmd_args()

    # Determine the compiler type specified by the user
    for t in CompilerType:
        if t.value == build_args.compiler_type:
            cmp_type = t
            break
    else:
        raise ValueError(
            f"\"{build_args.compiler_type}\" is not a valid compiler type"
        )
    # Determine the build type specified by the user
    for t in BuildType:
        if t.value == build_args.build_type:
            build_type = t
            break
    else:
        raise ValueError(
            f"\"{build_args.build_type}\" is not a valid build type"
        )
    
    if (        build_args.build_type == BuildType.NOBUILD
            and build_args.clean == "none"):
        raise ValueError(
            f"Choose something! Run: `{__file__} --help` for more information"
        )
    
    if build_type != BuildType.NOBUILD:
        # Build the external libraries
        ext_lib_path_list = build_external_lib(cmp_type, build_type)
        if ext_lib_path_list:
            ext_lib_path_list.insert(0, "-libs")
        
        # Build the rest of the project.
        fobis_build_flag_list = get_fobis_build_flags(
                cmp_type, build_type, flag_type="build"
            )
        fobis_build_flag_str = " ".join(
                ext_lib_path_list + fobis_build_flag_list
            )

        print("\n[Building the project]\n")
        os.system(f"{FOBIS} build {fobis_build_flag_str}")

    if build_args.clean != "none":
        clean(build_args.clean)


if __name__ == "__main__":
    main()