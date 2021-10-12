#!/usr/bin/env python
# SPDX-Identifier: BSD-2-Clause

from os import environ, listdir, makedirs
from os.path import join, isdir, exists
from sys import argv
from shutil import copy

if "MESON_INSTALL_DESTDIR_PREFIX" in environ:
    install_dir = environ["MESON_INSTALL_DESTDIR_PREFIX"]
else:
    install_dir = environ["MESON_INSTALL_PREFIX"]

include_dir = argv[1] if len(argv) > 1 else "include"
build_dir = argv[2] if len(argv) > 2 else environ["MESON_BUILD_ROOT"]
module_dir = join(install_dir, include_dir)

modules = []
for d in listdir(build_dir):
    bd = join(build_dir, d)
    if isdir(bd):
        for f in listdir(bd):
            if f.endswith(".mod"):
                modules.append(join(bd, f))

if not exists(module_dir):
    makedirs(module_dir)

for mod in modules:
    print("Installing", mod, "to", module_dir)
    copy(mod, module_dir)

