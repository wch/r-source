BEGIN { active = 0; n = 0; }
{
  if ($0 ~ /^Package:/) {
    package = $2
  } else if ($0 ~ /^Title:/) {
    active = 1
    n = NF - 1
    for (i = 2; i <= NF; i++) {
      a[i - 1] = $i
    }
  } else if ($0 ~ /^[ \t]/) {
    if (active == 1) {
      for (i = 1; i <= NF; i++) {
	a[n + i] = $i
      }
      n = n + NF
    }
  } else
    active = 0
}
END {
  s = ""
  lc = 16
  rc = 72
  old = package
  len = length(old)
  if (len > lc - 3) {
    s = old
    for (j = 1; j < lc; j++) { old = old " " }
  } else {
    for (j = 1; j < lc - len; j++) { old = old " " }
  }
  for (i = 1; i <= n; i++) {
    new = old " " a[i]
    if (length(new) > rc) {
      if (length(s) > 0) {
        s = s "\n" old
      } else {
        s = old
      }
      old = ""
      for (j = 1; j < lc; j++) { old = old " " }
      new = old " " a[i]
    }
    old = new
  }
  if (length(s) > 0) {
    s = s "\n" new
  } else {
    s = old
  }
  print s
}
