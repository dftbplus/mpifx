# SPDX-Identifier: BSD-2-Clause

sources_fpp = files(
  'module.fpp',
  'mpifx_abort.fpp',
  'mpifx_allgather.fpp',
  'mpifx_allgatherv.fpp',
  'mpifx_allreduce.fpp',
  'mpifx_barrier.fpp',
  'mpifx_bcast.fpp',
  'mpifx_comm.fpp',
  'mpifx_constants.fpp',
  'mpifx_finalize.fpp',
  'mpifx_gather.fpp',
  'mpifx_gatherv.fpp',
  'mpifx_get_processor_name.fpp',
  'mpifx_helper.fpp',
  'mpifx_init.fpp',
  'mpifx_recv.fpp',
  'mpifx_reduce.fpp',
  'mpifx_scatter.fpp',
  'mpifx_scatterv.fpp',
  'mpifx_send.fpp',
  'mpifx_win.fpp',
)
sources_f90 = []
foreach src : sources_fpp
  sources_f90 += configure_file(
    command: ['fypp', '@INPUT@', '@OUTPUT@'],
    input: src,
    output: '@BASENAME@.f90',
  )
endforeach

mpifx_lib = library(
  meson.project_name(),
  sources: sources_f90,
  dependencies: mpi_dep,
  install: install,
)

mpifx_inc = mpifx_lib.private_dir_include()
mpifx_dep = declare_dependency(
  link_with: mpifx_lib,
  include_directories: mpifx_inc,
  dependencies: mpi_dep,
)

if install
  module_id = meson.project_name()
  meson.add_install_script(
    '../cmake/install-mod.py',
    get_option('includedir') / module_id,
    meson.current_build_dir(),
  )

  pkg = import('pkgconfig')
  pkg.generate(
    mpifx_lib,
    description: 'Modern Fortran Interface for MPI',
    subdirs: ['', module_id],
  )
endif
