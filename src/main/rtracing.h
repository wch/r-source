/*
 * Internal tracing helpers for optional runtime probes.
 *
 * DTrace probes are generated from Rtrace.d into Rtrace.h.
 * Linux eBPF userspace probes use sys/sdt.h (STAP_PROBE* macros).
 */

#ifndef R_MAIN_RTRACING_H
#define R_MAIN_RTRACING_H

#if defined(HAVE_DTRACE)

#if defined(__has_include)
#if __has_include("Rtrace.h")
#include "Rtrace.h"
#define R_HAVE_GENERATED_DTRACE_HEADER 1
#endif
#endif
#if !defined(R_HAVE_GENERATED_DTRACE_HEADER)
#define R_HAVE_GENERATED_DTRACE_HEADER 0
#endif

#if R_HAVE_GENERATED_DTRACE_HEADER

#define R_TRACE_EVAL_ENTRY(expr, depth) \
    RTRACE_EVAL_ENTRY((char *)(expr), (int)(depth))
#define R_TRACE_EVAL_DISPATCH(kind, target) \
    RTRACE_EVAL_DISPATCH((char *)(kind), (char *)(target))

#define R_TRACE_NATIVE_ENTRY(kind, name) \
    RTRACE_NATIVE_ENTRY((char *)(kind), (char *)(name))
#define R_TRACE_NATIVE_EXIT(kind, name) \
    RTRACE_NATIVE_EXIT((char *)(kind), (char *)(name))

#define R_TRACE_GC_START(size_needed, gc_count) \
    RTRACE_GC_START((unsigned long long)(size_needed), (int)(gc_count))
#define R_TRACE_GC_END(gens_collected, gc_count) \
    RTRACE_GC_END((int)(gens_collected), (int)(gc_count))

#else

#define R_TRACE_EVAL_ENTRY(expr, depth) ((void)0)
#define R_TRACE_EVAL_DISPATCH(kind, target) ((void)0)

#define R_TRACE_NATIVE_ENTRY(kind, name) ((void)0)
#define R_TRACE_NATIVE_EXIT(kind, name) ((void)0)

#define R_TRACE_GC_START(size_needed, gc_count) ((void)0)
#define R_TRACE_GC_END(gens_collected, gc_count) ((void)0)

#endif

#elif defined(HAVE_EBPF_USDT)

#if defined(__has_include)
#if __has_include(<sys/sdt.h>)
#include <sys/sdt.h>
#endif
#endif

#ifndef STAP_PROBE1
#define STAP_PROBE1(provider, name, arg1) ((void)0)
#endif
#ifndef STAP_PROBE2
#define STAP_PROBE2(provider, name, arg1, arg2) ((void)0)
#endif

#define R_TRACE_EVAL_ENTRY(expr, depth) \
    STAP_PROBE2(rtrace, eval_entry, (expr), (depth))
#define R_TRACE_EVAL_DISPATCH(kind, target) \
    STAP_PROBE2(rtrace, eval_dispatch, (kind), (target))

#define R_TRACE_NATIVE_ENTRY(kind, name) \
    STAP_PROBE2(rtrace, native_entry, (kind), (name))
#define R_TRACE_NATIVE_EXIT(kind, name) \
    STAP_PROBE2(rtrace, native_exit, (kind), (name))

#define R_TRACE_GC_START(size_needed, gc_count) \
    STAP_PROBE2(rtrace, gc_start, (size_needed), (gc_count))
#define R_TRACE_GC_END(gens_collected, gc_count) \
    STAP_PROBE2(rtrace, gc_end, (gens_collected), (gc_count))

#else

#define R_TRACE_EVAL_ENTRY(expr, depth) ((void)0)
#define R_TRACE_EVAL_DISPATCH(kind, target) ((void)0)

#define R_TRACE_NATIVE_ENTRY(kind, name) ((void)0)
#define R_TRACE_NATIVE_EXIT(kind, name) ((void)0)

#define R_TRACE_GC_START(size_needed, gc_count) ((void)0)
#define R_TRACE_GC_END(gens_collected, gc_count) ((void)0)

#endif

#endif
