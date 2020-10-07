// runme with command `jsonnet stdlib-list.jsonnet -S`

std.join(
  ' ',
  std.sort(
    std.map(
      function(f) '"' + f + '"',
      std.objectFieldsAll(std)
    )
  )
)
