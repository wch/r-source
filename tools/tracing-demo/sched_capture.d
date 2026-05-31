#pragma D option quiet

dtrace:::BEGIN
{
  printf("wall_ns\tevent\tvalue_us\tpid\ttid\tcpu\n");
}

profile-199
/pid == $target/
{
  printf("%lld\toncpu-sample\t0\t%d\t%d\t%d\n",
      walltimestamp,
      pid,
      tid,
      cpu);
}
