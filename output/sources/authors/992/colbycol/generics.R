#################################################################
# 
# File:         generics.R
# Purpose:      Adds new generics to extending some base package functions
#
# Created:      20090509
# Author:       Carlos J. Gil Bellosta
#
# Modifications: 
#
#################################################################

ncol <- function( x ) UseMethod("ncol")
ncol.default <- base::ncol

nrow <- function( x ) UseMethod("nrow")
nrow.default <- base::nrow

colnames <- function( x, do.NULL = TRUE, prefix = "col" ) UseMethod("colnames")
colnames.default <- base::colnames
