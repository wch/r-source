% File src/library/grid/man/grid-internal.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{grid-internal}
%% <FIXME>
%% These had aliases but no corresponding code object:
%% <LIST>
%% \alias{.unit}
%% \alias{.unit.arithmetic}
%% \alias{.unit.list}
%% \alias{clear.display.list}
%% \alias{clearpage}
%% \alias{common.draw.axis}
%% \alias{convert.gpar}
%% \alias{copy}
%% \alias{demo.locator}
%% \alias{editDetails.frame}
%% \alias{grid.height}
%% \alias{grid.init.viewport.stack}
%% \alias{height.default}
%% \alias{height.frame}
%% \alias{height.lines}
%% \alias{height.text}
%% \alias{justifyX}
%% \alias{justifyY}
%% \alias{pop.saved.Rpars}
%% \alias{push.saved.Rpars}
%% \alias{set.viewport}
%% \alias{stack.viewports}
%% \alias{unit.list.c}
%% \alias{unstack.viewports}
%% \alias{viewport.layout<-}
%% \alias{width.default}
%% \alias{width.frame}
%% \alias{width.lines}
%% \alias{width.text}
%% \alias{grid.width}
%% </LIST>
%% </FIXME>

%% These are now registered but unexported S3 methods:
%% <LIST>
%% \alias{[.unit}
%% \alias{[.unit.arithmetic}
%% \alias{[.unit.list}
%% \alias{Ops.unit}
%% \alias{Summary.unit}
%% \alias{as.character.unit}
%% \alias{as.character.unit.arithmetic}
%% \alias{as.character.unit.list}
%% \alias{print.grob}
%% \alias{print.unit}
%% \alias{print.viewport}
%% \alias{draw.details.arrows}
%% \alias{draw.details.circle}
%% \alias{draw.details.collection}
%% \alias{draw.details.default}
%% \alias{draw.details.frame}
%% \alias{draw.details.glist}
%% \alias{draw.details.lines}
%% \alias{draw.details.line.to}
%% \alias{draw.details.move.to}
%% \alias{draw.details.points}
%% \alias{draw.details.polygon}
%% \alias{draw.details.rect}
%% \alias{draw.details.segments}
%% \alias{draw.details.text}
%% \alias{draw.details.viewport}
%% \alias{draw.details.xaxis}
%% \alias{draw.details.yaxis}
%% \alias{editDetails.default}
%% \alias{editDetails.xaxis}
%% \alias{editDetails.yaxis}
%% \alias{height.details.default}
%% \alias{height.details.frame}
%% \alias{height.details.rect}
%% \alias{height.details.text}
%% \alias{height.details.viewport}
%% \alias{height.post.details.default}
%% \alias{height.pre.details.default}
%% \alias{width.details.default}
%% \alias{width.details.frame}
%% \alias{width.details.rect}
%% \alias{width.details.text}
%% \alias{width.details.viewport}
%% \alias{width.post.details.default}
%% \alias{width.pre.details.default}
%% </LIST>

%% These are no longer in the namespace:
%% <LIST>
%% \alias{.GRID.STATE}
%% \alias{.grid.gpar.names}
%% \alias{.grid.loaded}
%% \alias{.grid.unit.list}
%% \alias{absolute}
%% \alias{absolute.units}
%% \alias{absolute.units.unit}
%% \alias{absolute.units.unit.arithmetic}
%% \alias{absolute.units.unit.list}
%% \alias{col.spec}
%% \alias{draw.all}
%% \alias{draw.frame.child}
%% \alias{get.gpar}
%% \alias{get.value}
%% \alias{get.value.default}
%% \alias{get.value.grob}
%% \alias{grid.Call}
%% \alias{grid.Call.graphics}
%% \alias{grid.top.level.vp}
%% \alias{height}
%% \alias{height.post}
%% \alias{height.pre}
%% \alias{inc.display.list}
%% \alias{is.even}
%% \alias{is.gpar}
%% \alias{is.grob}
%% \alias{is.layout}
%% \alias{is.odd}
%% \alias{is.unit.arithmetic}
%% \alias{is.unit.list}
%% \alias{is.viewport}
%% \alias{layout.heights}
%% \alias{layout.ncol}
%% \alias{layout.nrow}
%% \alias{layout.respect}
%% \alias{layout.widths}
%% \alias{make.xaxis.labels}
%% \alias{make.xaxis.major}
%% \alias{make.xaxis.ticks}
%% \alias{make.yaxis.labels}
%% \alias{make.yaxis.major}
%% \alias{make.yaxis.ticks}
%% \alias{mod.dims}
%% \alias{new.col}
%% \alias{new.row}
%% \alias{num.col.specs}
%% \alias{num.row.specs}
%% \alias{origin.bottom}
%% \alias{origin.left}
%% \alias{origin.right}
%% \alias{origin.top}
%% \alias{pop.saved.gpars}
%% \alias{pop.vp}
%% \alias{push.saved.gpars}
%% \alias{push.vp}
%% \alias{record}
%% \alias{record.default}
%% \alias{record.grob}
%% \alias{record.viewport}
%% \alias{recycle.data}
%% \alias{row.spec}
%% \alias{saved.pars}
%% \alias{set.gpar}
%% \alias{set.value.grob}
%% \alias{swap.origin.horizontal}
%% \alias{swap.origin.vertical}
%% \alias{unit.arithmetic}
%% \alias{unit.arithmetic.rep}
%% \alias{unit.length.unit}
%% \alias{unit.length.unit.arithmetic}
%% \alias{unit.length.unit.list}
%% \alias{unit.list}
%% \alias{unit.list.from.list}
%% \alias{unit.list.rep}
%% \alias{unset.gpar}
%% \alias{updateCol}
%% \alias{updateRow}
%% \alias{valid.data}
%% \alias{valid.just}
%% \alias{valid.layout}
%% \alias{valid.origin}
%% \alias{valid.pch}
%% \alias{valid.unit}
%% \alias{valid.units}
%% \alias{valid.viewport}
%% \alias{validGP}
%% \alias{width}
%% \alias{width.post}
%% \alias{width.pre}
%% </LIST>

%% Part of the sensible export list but (currently?) documented as
%% internal (or waiting for documentation to be written):
%% <LIST>
\alias{grid.multipanel}
\alias{grid.panel}
\alias{grid.strip}
\alias{is.unit}
\alias{layoutRegion}
\alias{viewport.layout}
\alias{layout.heights}
\alias{layout.widths}
%% </LIST>
%% Currently (?) exported as used in the grid.layout.Rd \example ...
%% <LIST>
\alias{layout.torture}
%% </LIST>

\title{Internal Grid Functions}
\description{
  Internal Grid functions
}
\details{
  These are not to be called by the user (or in some cases are just
  waiting for proper documentation to be written :).
}
\keyword{ internal }
