/*
 *  R : A Computer Langage for Statistical Data Analysis
 *  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* Information for Deparsing Expressions */

#define PP_ASSIGN	 1
#define PP_ASSIGN2	 2
#define PP_BINARY	 3
#define PP_BINARY2	 4
#define PP_BREAK	 5
#define PP_CURLY	 6
#define PP_FOR		 7
#define PP_FUNCALL	 8
#define PP_FUNCTION	 9
#define PP_IF		10
#define PP_NEXT		11
#define PP_PAREN	12
#define PP_RETURN	13
#define PP_SUBASS	14
#define PP_SUBSET	15
#define PP_WHILE	16
#define PP_UNARY	17
#define PP_DOLLAR	18
#define PP_FOREIGN	19
#define PP_REPEAT	20

/* Device drivers here (for ease of access) */

SEXP do_X11(SEXP, SEXP, SEXP, SEXP);
SEXP do_PS(SEXP, SEXP, SEXP, SEXP);
SEXP do_PicTeX(SEXP, SEXP, SEXP, SEXP);
SEXP do_Gnome(SEXP, SEXP, SEXP, SEXP);

/* Function Names */

#ifdef Unix
SEXP do_getenv(SEXP, SEXP, SEXP, SEXP);
#endif

#ifdef Win32
SEXP do_getenv(SEXP, SEXP, SEXP, SEXP);
SEXP do_tempfile(SEXP, SEXP, SEXP, SEXP);
SEXP do_unlink(SEXP, SEXP, SEXP, SEXP);
SEXP do_helpstart(SEXP, SEXP, SEXP, SEXP);
SEXP do_helpitem(SEXP, SEXP, SEXP, SEXP);
SEXP do_flushconsole(SEXP, SEXP, SEXP, SEXP);
SEXP do_int_unzip(SEXP, SEXP, SEXP, SEXP);
SEXP do_winver(SEXP, SEXP, SEXP, SEXP);
SEXP do_devga(SEXP, SEXP, SEXP, SEXP);
SEXP do_saveDevga(SEXP, SEXP, SEXP, SEXP);
#endif
#ifdef oldWin32
SEXP do_winedit(SEXP, SEXP, SEXP, SEXP);
SEXP do_sysfile(SEXP, SEXP, SEXP, SEXP);
SEXP do_getenv(SEXP, SEXP, SEXP, SEXP);
#endif

SEXP do_abbrev(SEXP, SEXP, SEXP, SEXP);
SEXP do_alias(SEXP, SEXP, SEXP, SEXP);
SEXP do_allnames(SEXP, SEXP, SEXP, SEXP);
SEXP do_anydf(SEXP, SEXP, SEXP, SEXP);
SEXP do_args(SEXP, SEXP, SEXP, SEXP);
SEXP do_array(SEXP, SEXP, SEXP, SEXP);
SEXP do_aperm(SEXP, SEXP, SEXP, SEXP);
SEXP do_arith(SEXP, SEXP, SEXP, SEXP);
SEXP do_arrows(SEXP, SEXP, SEXP, SEXP);
SEXP do_ascall(SEXP, SEXP, SEXP, SEXP);
SEXP do_asfunction(SEXP, SEXP, SEXP, SEXP);
SEXP do_asmatrixdf(SEXP, SEXP, SEXP, SEXP);
SEXP do_assign(SEXP, SEXP, SEXP, SEXP);
SEXP do_asvector(SEXP, SEXP, SEXP, SEXP);
SEXP do_atan(SEXP, SEXP, SEXP, SEXP);
SEXP do_attach(SEXP,SEXP,SEXP,SEXP);
SEXP do_attr(SEXP, SEXP, SEXP, SEXP);
SEXP do_attrgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_attributes(SEXP, SEXP, SEXP, SEXP);
SEXP do_attributesgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_axis(SEXP, SEXP, SEXP, SEXP);
SEXP do_basename(SEXP, SEXP, SEXP, SEXP);
SEXP do_begin(SEXP, SEXP, SEXP, SEXP);
SEXP do_bind(SEXP, SEXP, SEXP, SEXP);
SEXP do_body(SEXP, SEXP, SEXP, SEXP);
SEXP do_box(SEXP, SEXP, SEXP, SEXP);
SEXP do_break(SEXP, SEXP, SEXP, SEXP);
SEXP do_browser(SEXP, SEXP, SEXP, SEXP);
SEXP do_builtins(SEXP, SEXP, SEXP, SEXP);
SEXP do_c(SEXP, SEXP, SEXP, SEXP);
SEXP do_call(SEXP, SEXP, SEXP, SEXP);
SEXP do_cat(SEXP, SEXP, SEXP, SEXP);
SEXP do_charmatch(SEXP, SEXP, SEXP, SEXP);
SEXP do_class(SEXP, SEXP, SEXP, SEXP);
SEXP do_classgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_colors(SEXP, SEXP, SEXP, SEXP);
SEXP do_commandArgs(SEXP, SEXP, SEXP, SEXP);
SEXP do_comment(SEXP, SEXP, SEXP, SEXP);
SEXP do_commentgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_complex(SEXP, SEXP, SEXP, SEXP);
SEXP do_contour(SEXP, SEXP, SEXP, SEXP);
SEXP do_countfields(SEXP, SEXP, SEXP, SEXP);
SEXP do_cum(SEXP, SEXP, SEXP, SEXP);
SEXP do_compcases(SEXP, SEXP, SEXP, SEXP);
SEXP do_cov(SEXP, SEXP, SEXP, SEXP);
SEXP do_D(SEXP, SEXP, SEXP, SEXP);
SEXP do_dataentry(SEXP, SEXP, SEXP, SEXP);
SEXP do_dataframe(SEXP, SEXP, SEXP, SEXP);
SEXP do_date(SEXP, SEXP, SEXP, SEXP);
SEXP do_debug(SEXP, SEXP, SEXP, SEXP);
SEXP do_delay(SEXP, SEXP, SEXP, SEXP);
SEXP do_dend(SEXP, SEXP, SEXP, SEXP);
SEXP do_dendwindow(SEXP, SEXP, SEXP, SEXP);
SEXP do_deparse(SEXP, SEXP, SEXP, SEXP);
SEXP do_deriv(SEXP, SEXP, SEXP, SEXP);
SEXP do_detach(SEXP,SEXP,SEXP,SEXP);
SEXP do_devcontrol(SEXP,SEXP,SEXP,SEXP);
SEXP do_devcopy(SEXP,SEXP,SEXP,SEXP);
SEXP do_devcur(SEXP, SEXP, SEXP, SEXP);
SEXP do_device(SEXP, SEXP, SEXP, SEXP);
SEXP do_devnext(SEXP, SEXP, SEXP, SEXP);
SEXP do_devoff(SEXP, SEXP, SEXP, SEXP);
SEXP do_devprev(SEXP, SEXP, SEXP, SEXP);
SEXP do_devset(SEXP, SEXP, SEXP, SEXP);
SEXP do_dim(SEXP, SEXP, SEXP, SEXP);
SEXP do_dimgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_dimnames(SEXP, SEXP, SEXP, SEXP);
SEXP do_dimnamesgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_dirname(SEXP, SEXP, SEXP, SEXP);
SEXP do_docall(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotcall(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotCode(SEXP, SEXP, SEXP, SEXP);
SEXP do_dotplot(SEXP, SEXP, SEXP, SEXP);
SEXP do_dput(SEXP, SEXP, SEXP, SEXP);
SEXP do_drop(SEXP, SEXP, SEXP, SEXP);
SEXP do_dumpb(SEXP, SEXP, SEXP, SEXP);
SEXP do_dump(SEXP, SEXP, SEXP, SEXP);
SEXP do_duplicated(SEXP, SEXP, SEXP, SEXP);
SEXP do_dynload(SEXP, SEXP, SEXP, SEXP);
SEXP do_dynunload(SEXP, SEXP, SEXP, SEXP);
SEXP do_edit(SEXP, SEXP, SEXP, SEXP);
SEXP do_envir(SEXP, SEXP, SEXP, SEXP);
SEXP do_envirgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_erase(SEXP, SEXP, SEXP, SEXP);
SEXP do_External(SEXP, SEXP, SEXP, SEXP);
SEXP do_eval(SEXP, SEXP, SEXP, SEXP);
SEXP do_expression(SEXP, SEXP, SEXP, SEXP);
SEXP do_fileappend(SEXP, SEXP, SEXP, SEXP);
SEXP do_filechoose(SEXP, SEXP, SEXP, SEXP);
SEXP do_fileexists(SEXP, SEXP, SEXP, SEXP);
SEXP do_filecreate(SEXP, SEXP, SEXP, SEXP);
SEXP do_fileremove(SEXP, SEXP, SEXP, SEXP);
SEXP do_fileshow(SEXP, SEXP, SEXP, SEXP);
SEXP do_fft(SEXP, SEXP, SEXP, SEXP);
SEXP do_fmin(SEXP, SEXP, SEXP, SEXP);
SEXP do_for(SEXP, SEXP, SEXP, SEXP);
SEXP do_format(SEXP, SEXP, SEXP, SEXP);
SEXP do_formatinfo(SEXP, SEXP, SEXP, SEXP);
SEXP do_formals(SEXP, SEXP, SEXP, SEXP);
SEXP do_function(SEXP, SEXP, SEXP, SEXP);
SEXP do_gc(SEXP, SEXP, SEXP, SEXP);
SEXP do_gcinfo(SEXP, SEXP, SEXP, SEXP);
SEXP do_gctorture(SEXP, SEXP, SEXP, SEXP);
SEXP do_get(SEXP, SEXP, SEXP, SEXP);
SEXP do_getwd(SEXP, SEXP, SEXP, SEXP);
SEXP do_globalenv(SEXP, SEXP, SEXP, SEXP);
SEXP do_gray(SEXP, SEXP, SEXP, SEXP);
SEXP do_grep(SEXP, SEXP, SEXP, SEXP);
SEXP do_gsub(SEXP, SEXP, SEXP, SEXP);
SEXP do_hdf5save (SEXP, SEXP, SEXP, SEXP);
SEXP do_hdf5load (SEXP, SEXP, SEXP, SEXP);
SEXP do_hsv(SEXP, SEXP, SEXP, SEXP);
SEXP do_identify(SEXP, SEXP, SEXP, SEXP);
SEXP do_if(SEXP, SEXP, SEXP, SEXP);
SEXP do_image(SEXP, SEXP, SEXP, SEXP);
SEXP do_interactive(SEXP, SEXP, SEXP, SEXP);
SEXP do_internal(SEXP, SEXP, SEXP, SEXP);
SEXP do_invisible(SEXP, SEXP, SEXP, SEXP);
SEXP do_is(SEXP, SEXP, SEXP, SEXP);
SEXP do_isfinite(SEXP, SEXP, SEXP, SEXP);
SEXP do_isinfinite(SEXP, SEXP, SEXP, SEXP);
SEXP do_isloaded(SEXP, SEXP, SEXP, SEXP);
SEXP do_isna(SEXP, SEXP, SEXP, SEXP);
SEXP do_isnan(SEXP, SEXP, SEXP, SEXP);
SEXP do_isvector(SEXP, SEXP, SEXP, SEXP);
SEXP do_layout(SEXP, SEXP, SEXP, SEXP);
SEXP do_length(SEXP, SEXP, SEXP, SEXP);
SEXP do_lengthgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_libfixup(SEXP, SEXP, SEXP, SEXP);
SEXP do_listfiles(SEXP, SEXP, SEXP, SEXP);
SEXP do_load (SEXP, SEXP, SEXP, SEXP);
SEXP do_locator(SEXP, SEXP, SEXP, SEXP);
SEXP do_log(SEXP, SEXP, SEXP, SEXP);
SEXP do_logic(SEXP, SEXP, SEXP, SEXP);
SEXP do_logic2(SEXP, SEXP, SEXP, SEXP);
SEXP do_logic3(SEXP, SEXP, SEXP, SEXP);
SEXP do_ls(SEXP, SEXP, SEXP, SEXP);
SEXP do_Machine(SEXP, SEXP, SEXP, SEXP);
SEXP do_machine(SEXP, SEXP, SEXP, SEXP);
SEXP do_Macintosh(SEXP, SEXP, SEXP, SEXP);
SEXP do_makelist(SEXP, SEXP, SEXP, SEXP);
SEXP do_makenames(SEXP, SEXP, SEXP, SEXP);
SEXP do_makevector(SEXP, SEXP, SEXP, SEXP);
SEXP do_match(SEXP, SEXP, SEXP, SEXP);
SEXP do_matchcall(SEXP, SEXP, SEXP, SEXP);
SEXP do_matprod(SEXP, SEXP, SEXP, SEXP);
SEXP do_matrix(SEXP, SEXP, SEXP, SEXP);
SEXP do_memoryprofile(SEXP, SEXP, SEXP, SEXP);
SEXP do_menu(SEXP, SEXP, SEXP, SEXP);
SEXP do_modelframe(SEXP, SEXP, SEXP, SEXP);
SEXP do_modelmatrix(SEXP, SEXP, SEXP, SEXP);
SEXP do_missing(SEXP, SEXP, SEXP, SEXP);
SEXP do_mtext(SEXP, SEXP, SEXP, SEXP);
SEXP do_mvfft(SEXP, SEXP, SEXP, SEXP);
SEXP do_names(SEXP, SEXP, SEXP, SEXP);
SEXP do_namesgets(SEXP, SEXP, SEXP, SEXP);
SEXP do_nargs(SEXP, SEXP, SEXP, SEXP);
SEXP do_nchar(SEXP,SEXP,SEXP,SEXP);
SEXP do_nextmethod(SEXP,SEXP,SEXP,SEXP);
SEXP do_nextn(SEXP,SEXP,SEXP,SEXP);
SEXP do_nlm(SEXP, SEXP, SEXP, SEXP);
SEXP do_onexit(SEXP, SEXP, SEXP, SEXP);
SEXP do_options(SEXP, SEXP, SEXP, SEXP);
SEXP do_order(SEXP, SEXP, SEXP, SEXP);
SEXP do_palette(SEXP, SEXP, SEXP, SEXP);
SEXP do_par(SEXP, SEXP, SEXP, SEXP);
SEXP do_paren(SEXP, SEXP, SEXP, SEXP);
SEXP do_parse(SEXP, SEXP, SEXP, SEXP);
SEXP do_paste(SEXP, SEXP, SEXP, SEXP);
SEXP do_pause(SEXP, SEXP, SEXP, SEXP);
SEXP do_persp(SEXP, SEXP, SEXP, SEXP);
SEXP do_polyroot(SEXP, SEXP, SEXP, SEXP);
SEXP do_pos2env(SEXP, SEXP, SEXP, SEXP);
SEXP do_abline(SEXP, SEXP, SEXP, SEXP);
SEXP do_plot_xy(SEXP, SEXP, SEXP, SEXP);
SEXP do_plot_new(SEXP, SEXP, SEXP, SEXP);
SEXP do_plot_window(SEXP, SEXP, SEXP, SEXP);
SEXP do_title(SEXP, SEXP, SEXP, SEXP);
SEXP do_trace(SEXP, SEXP, SEXP, SEXP);
SEXP do_Platform(SEXP, SEXP, SEXP, SEXP);
SEXP do_pmatch(SEXP, SEXP, SEXP, SEXP);
SEXP do_polygon(SEXP, SEXP, SEXP, SEXP);
SEXP do_printdefault(SEXP, SEXP, SEXP, SEXP);
SEXP do_printdf(SEXP, SEXP, SEXP, SEXP);
SEXP do_printmatrix(SEXP, SEXP, SEXP, SEXP);
SEXP do_psort(SEXP, SEXP, SEXP, SEXP);
SEXP do_quit(SEXP, SEXP, SEXP, SEXP);
SEXP do_random1(SEXP, SEXP, SEXP, SEXP);
SEXP do_random2(SEXP, SEXP, SEXP, SEXP);
SEXP do_random3(SEXP, SEXP, SEXP, SEXP);
SEXP do_range(SEXP, SEXP, SEXP, SEXP);
SEXP do_rank(SEXP, SEXP, SEXP, SEXP);
SEXP do_readln(SEXP, SEXP, SEXP, SEXP);
SEXP do_recall(SEXP, SEXP, SEXP, SEXP);
SEXP do_rect(SEXP, SEXP, SEXP, SEXP);
SEXP do_relop(SEXP, SEXP, SEXP, SEXP);
SEXP do_remove(SEXP, SEXP, SEXP, SEXP);
SEXP do_rep(SEXP, SEXP, SEXP, SEXP);
SEXP do_repeat(SEXP, SEXP, SEXP, SEXP);
SEXP do_replay(SEXP, SEXP, SEXP, SEXP);
SEXP do_restoreb(SEXP, SEXP, SEXP, SEXP);
SEXP do_return(SEXP, SEXP, SEXP, SEXP);
SEXP do_rgb(SEXP, SEXP, SEXP, SEXP);
SEXP do_Rhome(SEXP, SEXP, SEXP, SEXP);
SEXP do_RNGkind(SEXP, SEXP, SEXP, SEXP);
SEXP do_round(SEXP, SEXP, SEXP, SEXP);
SEXP do_rownames(SEXP, SEXP, SEXP, SEXP);
SEXP do_rowscols(SEXP, SEXP, SEXP, SEXP);
SEXP do_sample(SEXP, SEXP, SEXP, SEXP);
SEXP do_save(SEXP, SEXP, SEXP, SEXP);
SEXP do_scan(SEXP, SEXP, SEXP, SEXP);
SEXP do_search(SEXP, SEXP, SEXP, SEXP);
SEXP do_segments(SEXP, SEXP, SEXP, SEXP);
SEXP do_seq(SEXP, SEXP, SEXP, SEXP);
SEXP do_set(SEXP, SEXP, SEXP, SEXP);
SEXP do_setwd(SEXP, SEXP, SEXP, SEXP);
SEXP do_shade(SEXP, SEXP, SEXP, SEXP);
SEXP do_signif(SEXP, SEXP, SEXP, SEXP);
SEXP do_strheight(SEXP, SEXP, SEXP, SEXP);
SEXP do_strwidth(SEXP, SEXP, SEXP, SEXP);
SEXP do_sink(SEXP, SEXP, SEXP, SEXP);
SEXP do_sort(SEXP, SEXP, SEXP, SEXP);
SEXP do_split(SEXP, SEXP, SEXP, SEXP);
SEXP do_stop(SEXP, SEXP, SEXP, SEXP);
SEXP do_strsplit(SEXP,SEXP,SEXP,SEXP);
SEXP do_subassign(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign2(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassign3(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassigndf(SEXP, SEXP, SEXP, SEXP);
SEXP do_subassigndf2(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset2(SEXP, SEXP, SEXP, SEXP);
SEXP do_subset3(SEXP, SEXP, SEXP, SEXP);
SEXP do_subsetdf(SEXP, SEXP, SEXP, SEXP);
SEXP do_subsetdf2(SEXP, SEXP, SEXP, SEXP);
SEXP do_substitute(SEXP, SEXP, SEXP, SEXP);
SEXP do_substr(SEXP,SEXP,SEXP,SEXP);
SEXP do_summary(SEXP, SEXP, SEXP, SEXP);
SEXP do_switch(SEXP, SEXP, SEXP, SEXP);
SEXP do_symbol(SEXP, SEXP, SEXP, SEXP);
SEXP do_sys(SEXP, SEXP, SEXP, SEXP);
SEXP do_system(SEXP, SEXP, SEXP, SEXP);
SEXP do_termsform(SEXP, SEXP, SEXP, SEXP);
SEXP do_text(SEXP, SEXP, SEXP, SEXP);
SEXP do_tilde(SEXP, SEXP, SEXP, SEXP);
#ifdef HAVE_TIMES
SEXP do_proctime(SEXP, SEXP, SEXP, SEXP);
#endif
SEXP do_transpose(SEXP, SEXP, SEXP, SEXP);
SEXP do_typecvt(SEXP, SEXP, SEXP, SEXP);
SEXP do_typeof(SEXP, SEXP, SEXP, SEXP);
SEXP do_unclass(SEXP, SEXP, SEXP, SEXP);
SEXP do_unlist(SEXP, SEXP, SEXP, SEXP);
SEXP do_updateform(SEXP, SEXP, SEXP, SEXP);
SEXP do_usemethod(SEXP, SEXP, SEXP, SEXP);
SEXP do_version(SEXP, SEXP, SEXP, SEXP);
SEXP do_warning(SEXP, SEXP, SEXP, SEXP);
SEXP do_while(SEXP, SEXP, SEXP, SEXP);
SEXP do_zeroin(SEXP, SEXP, SEXP, SEXP);
