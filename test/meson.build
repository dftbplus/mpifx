# SPDX-Identifier: BSD-2-Clause

tests = [
  'allgather',
  'allgatherv',
  'allreduce',
  'bcast',
  'comm_split',
  'comm_split_type',
  'gather',
  'gatherv',
  'reduce',
  'scatter',
  'scatterv',
  'shared_memory',
]

foreach t : tests
  executable(
    'test_@0@'.format(t),
    sources: files('test_@0@.f90'.format(t)),
    dependencies: mpifx_dep,
  )
endforeach
