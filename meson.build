# SPDX-Identifier: BSD-2-Clause

project(
  'mpifx',
  'Fortran',
  version: files('VERSION'),
  license: 'BSD-2-Clause',
  meson_version: '>=0.57.2',
  default_options: [
    'buildtype=debugoptimized',
    'default_library=both',
  ],
)
install = not (meson.is_subproject() and get_option('default_library') == 'static')

# Prerequisites
mpi_dep = dependency('mpi', language: 'fortran')
fypp = find_program('fypp')

# Build instructions
subdir('lib')

mpifx_lic = files('LICENSE')

if install
  install_data(
    mpifx_lic,
    install_dir: get_option('datadir')/'licenses'/meson.project_name()
  )
endif
