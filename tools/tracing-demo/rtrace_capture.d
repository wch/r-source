#pragma D option quiet

dtrace:::BEGIN
{
  printf("wall_ns\tprobe\tpid\ttid\tcpu\targ0\targ1\targ2\n");
}

rtrace*:::*
/pid == $target/
{
  printf("%lld\t%s\t%d\t%d\t%d\t0x%llx\t0x%llx\t0x%llx\n",
      walltimestamp,
      probename,
      pid,
      tid,
      cpu,
      (unsigned long long)arg0,
      (unsigned long long)arg1,
      (unsigned long long)arg2);
}
