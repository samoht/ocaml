#!/bin/sh
set -eu

mode=${1:?generation mode required}
output=${2:?output file required}
count=${3:?item count required}

case "$mode" in
  live-functions)
    {
      i=0
      while test "$i" -lt "$count"; do
        printf 'let f_%d x = ((x + %d) * 3) land 65535\n' "$i" "$i"
        i=$((i + 1))
      done
      printf 'external opaque : int -> int = "%%opaque"\n'
      printf 'let table = [|\n'
      i=0
      while test "$i" -lt "$count"; do
        printf '  f_%d;\n' "$i"
        i=$((i + 1))
      done
      printf '|]\n'
      printf 'let () = Printf.printf "large %%d\\n" (table.(opaque 0) 42)\n'
    } > "$output"
    ;;
  format-sites)
    {
      i=0
      while test "$i" -lt "$count"; do
        printf 'let () = Printf.printf "site %d = %%d\\n" %d\n' "$i" "$i"
        i=$((i + 1))
      done
    } > "$output"
    ;;
  *)
    printf 'unknown generation mode: %s\n' "$mode" >&2
    exit 2
    ;;
esac
