
# vclVector Inner (Dot) Product
vclVecInner <- function(A, B){
    
    assert_are_identical(A@.context_index, B@.context_index)
    
    type <- typeof(A)
    
    out <- switch(type,
                  integer = {
                      stop("OpenCL integer dot product not currently
                    supported for viennacl matrices")
                      #cpp_vclVector_igemm(A@address,
                      #                   B@address, 
                      #                   C@address,
                      #                   kernel)
                      #                      cpp_vclVector_igemm(A@address,
                      #                                           B@address,
                      #                                           C@address)
                  },
                  float = {cpp_vclVector_inner_prod(A@address,
                                                    B@address,
                                                    6L)
                  },
                  double = {
                      cpp_vclVector_inner_prod(A@address,
                                               B@address,
                                               8L)
                  
                  },
                  stop("type not recognized")
    )
    
    return(as.matrix(out))
}


# vclVector Outer Product
vclVecOuter <- function(A, B){
    
    # if(length(B) != length(A)) stop("Non conformant arguments")
    
    assert_are_identical(A@.context_index, B@.context_index)
    
    type <- typeof(A)
    
    C <- vclMatrix(nrow=length(A), ncol=length(B), type=type, ctx_id = A@.context_index)
    
    switch(type,
           integer = {
               stop("OpenCL integer outer product not currently
                    supported for viennacl matrices")
               #cpp_vclVector_igemm(A@address,
               #                   B@address, 
               #                   C@address,
               #                   kernel)
               #                      cpp_vclVector_igemm(A@address,
               #                                                        B@address,
               #                                                        C@address)
           },
           float = {cpp_vclVector_outer_prod(A@address,
                                             B@address,
                                             C@address,
                                             6L)
           },
           double = {
               cpp_vclVector_outer_prod(A@address,
                                        B@address,
                                        C@address,
                                        8L)
           },
           stop("type not recognized")
    )
    return(C)
}


# vclVector AXPY
vclVec_axpy <- function(alpha, A, B, inplace = FALSE, order = 0){
    
    assert_are_identical(A@.context_index, B@.context_index)
    
    type <- typeof(A)
    
    if(inplace){
        Z <- B 
    }else{
        Z <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
        if(!missing(B))
        {
            if(length(B) != length(A)) stop("Lengths of matrices must match")
            Z <- deepcopy(B)
        }   
    }
    
    switch(type,
           integer = {
               stop("OpenCL integer AXPY not currently
                    supported for viennacl matrices")
               #cpp_vclVector_iaxpy(alpha, 
               #                          A@address,
               #                          Z@address, 
               #                          kernel)
           },
           float = {cpp_vclVector_axpy(alpha, 
                                       A@address, 
                                       Z@address,
                                       order,
                                       6L)
           },
           double = {
               cpp_vclVector_axpy(alpha, 
                                  A@address,
                                  Z@address,
                                  order,
                                  8L)
           },
           stop("type not recognized")
    )
    
    if(inplace){
        return(invisible(Z))
    }else{
        return(Z)
    }
}


# GPU axpy wrapper
vclVector_unary_axpy <- function(A){
    
    type <- typeof(A)
    
    Z <- deepcopy(A)
    
    switch(type,
           integer = {
               cpp_vclVector_unary_axpy(Z@address, 4L)
           },
           float = {
               cpp_vclVector_unary_axpy(Z@address, 6L)
           },
           double = {
               cpp_vclVector_unary_axpy(Z@address, 8L)
           },
           stop("type not recognized")
    )
    
    return(Z)
}


# GPU Element-Wise Multiplication
vclVecElemMult <- function(A, B, inplace = FALSE){
    
    assert_are_identical(A@.context_index, B@.context_index)
    
    if( length(A) != length(B)){
        stop("Non-conformant arguments")
    }
    
    type <- typeof(A)
    
    if(inplace){
        C <- A
    }else{
        C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_prod(A@address,
                                            B@address,
                                            C@address,
                                            6L)
           },
           double = {
               cpp_vclVector_elem_prod(A@address,
                                       B@address,
                                       C@address,
                                       8L)
           },
           stop("type not recognized")
           )
    
    if(inplace){
        return(invisible(C))
    }else{
        return(C)    
    }
}


# GPU Scalar Element-Wise Multiplication
vclVecScalarMult <- function(A, B, inplace = FALSE){
    
    # quick class check when scalars are passed
    if(inherits(A, "vclVector")){
        
        type <- typeof(A)
        
        if(inplace){
            C <- A
        }else{
            C <- deepcopy(A)
        }    
        Z <- B
    }else{
        
        type <- typeof(B)
        
        if(inplace){
            C <- B
        }else{
            C <- deepcopy(B)
        }
        Z <- A
    }
    
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_scalar_prod(C@address,
                                              Z,
                                              6L)
           },
           double = {
               cpp_vclVector_scalar_prod(C@address,
                                         Z,
                                         8L)
           },
           stop("type not recognized")
    )
    
    if(inplace){
    	return(invisible(C))
    }else{
    	return(C)	
    }
}


# GPU Element-Wise Division
vclVecElemDiv <- function(A, B, inplace = FALSE){
    
    assert_are_identical(A@.context_index, B@.context_index)
    
    if( length(A) != length(B)){
        stop("Non-conformant arguments")
    }
    
    type <- typeof(A)
    
    if(inplace){
        C <- A
    }else{
        C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_div(A@address,
                                           B@address,
                                           C@address,
                                           6L)
           },
           double = {
               cpp_vclVector_elem_div(A@address,
                                      B@address,
                                      C@address,
                                      8L)
           },
           stop("type not recognized")
    )
    
    if(inplace){
        return(invisible(C))
    }else{
        return(C)    
    }
}


# GPU Scalar Element-Wise Division
vclVecScalarDiv <- function(A, B, order = 0, inplace = FALSE){
    
    # quick class check when scalars are passed
    if(inherits(A, "vclVector")){
        
        type <- typeof(A)
        
        if(inplace){
            C <- A
        }else{
            C <- deepcopy(A)
        }    
        Z <- B
    }else{
        
        type <- typeof(B)
        
        if(inplace){
            C <- B
        }else{
            C <- deepcopy(B)
        }
        Z <- A
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_scalar_div(C@address,
                                             Z,
                                             order,
                                             6L,
                                             C@.context_index - 1)
           },
           double = {
               cpp_vclVector_scalar_div(C@address,
                                        Z,
                                        order,
                                        8L,
                                        C@.context_index - 1)
           },
           stop("type not recognized")
    )
    
    if(inplace){
        return(invisible(C))
    }else{
        return(C)    
    }
}


# GPU Element-Wise Power
vclVecElemPow <- function(A, B){
    
    assert_are_identical(A@.context_index, B@.context_index)
    
    if(length(A) != length(B)){
        stop("arguments not conformable")
    }
    
    type <- typeof(A)
    
    C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_pow(A@address,
                                           B@address,
                                           C@address,
                                           6L)
           },
           double = {
               cpp_vclVector_elem_pow(A@address,
                                      B@address,
                                      C@address,
                                      8L)
           },
           stop("type not recognized")
    )
    return(C)
}


# GPU Element-Wise Power
vclVecScalarPow <- function(A, B){
    
    type <- typeof(A)
    
    C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_scalar_pow(A@address,
                                             B,
                                             C@address,
                                             6L)
           },
           double = {
               cpp_vclVector_scalar_pow(A@address,
                                        B,
                                        C@address,
                                        8L)
           },
           stop("type not recognized")
    )
    return(C)
}


# GPU Element-Wise sqrt
vclVecSqrt <- function(A){
    
    type <- typeof(A)
    
    C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_sqrt(A@address,
                                       C@address,
                                       6L)
           },
           double = {
               cpp_vclVector_sqrt(A@address,
                                  C@address,
                                  8L)
           },
           stop("type not recognized")
    )
    return(C)
}

# GPU Element-Wise Sine
vclVecElemSin <- function(A, inplace = FALSE){
    
    type <- typeof(A)
    
    if(inplace){
        C <- A
    }else{
        C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_sin(A@address,
                                           C@address,
                                           6L)
           },
           double = {
               cpp_vclVector_elem_sin(A@address,
                                      C@address,
                                      8L)
           },
           stop("type not recognized")
    )
    
    if(inplace){
        return(invisible(C))
    }else{
        return(C)    
    }
}


# GPU Element-Wise Arc Sine
vclVecElemArcSin <- function(A, inplace = FALSE){
    
    type <- typeof(A)

    if(inplace){
        C <- A
    }else{
        C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_asin(A@address,
                                            C@address,
                                            6L)
           },
           double = {
               cpp_vclVector_elem_asin(A@address,
                                       C@address,
                                       8L)
           },
           stop("type not recognized")
           )
    
    if(inplace){
        return(invisible(C))
    }else{
        return(C)    
    }
}


# GPU Element-Wise Hyperbolic Sine
vclVecElemHypSin <- function(A, inplace = FALSE){
    
    type <- typeof(A)
    
    if(inplace){
        C <- A
    }else{
        C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_sinh(A@address,
                                            C@address,
                                            6L)
           },
           double = {
               cpp_vclVector_elem_sinh(A@address,
                                       C@address,
                                       8L)
               
           },
           stop("type not recognized")
           )
    
    if(inplace){
        return(invisible(C))
    }else{
        return(C)
    }
}


# GPU Element-Wise Cos
vclVecElemCos <- function(A, inplace = FALSE){
    
    type <- typeof(A)
    
    if(inplace){
        C <- A
    }else{
        C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_cos(A@address,
                                           C@address,
                                           6L)
           },
           double = {
               cpp_vclVector_elem_cos(A@address,
                                      C@address,
                                      8L)
           },
           stop("type not recognized")
           )
    
    if(inplace){
        return(invisible(C))
    }else{
        return(C)    
    }
}


# GPU Element-Wise Arc Cos
vclVecElemArcCos <- function(A, inplace = FALSE){
    
    type <- typeof(A)
    
    if(inplace){
        C <- A
    }else{
        C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_acos(A@address,
                                            C@address,
                                            6L)
           },
           double = {
               cpp_vclVector_elem_acos(A@address,
                                       C@address,
                                       8L)
           },           
           stop("type not recognized")
    )
    
    if(inplace){
        return(invisible(C))
    }else{
        return(C)    
    }
}


# GPU Element-Wise Hyperbolic Cos
vclVecElemHypCos <- function(A, inplace = FALSE){
    
    type <- typeof(A)
    
    if(inplace){
        C <- A
    }else{
        C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_cosh(A@address,
                                            C@address,
                                            6L)
           },
           double = {
               cpp_vclVector_elem_cosh(A@address,
                                       C@address,
                                       8L)
           },
           stop("type not recognized")
    )
    
    if(inplace){
        return(invisible(C))
    }else{
        return(C)
    }
}


# GPU Element-Wise Tan
vclVecElemTan <- function(A, inplace = FALSE){
    
    type <- typeof(A)
    
    if(inplace){
        C <- A
    }else{
        C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_tan(A@address,
                                            C@address,
                                            6L)
           },
           double = {
               cpp_vclVector_elem_tan(A@address,
                                             C@address,
                                             8L)
           },
           stop("type not recognized")
    )
    
    if(inplace){
        return(invisible(C))
    }else{
        return(C)    
    }
}


# GPU Element-Wise Arc Tan
vclVecElemArcTan <- function(A, inplace = FALSE){
    
    type <- typeof(A)
    
    if(inplace){
       C <- A 
    }else{
        C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_atan(A@address,
                                             C@address,
                                             6L)
           },
           double = {
               cpp_vclVector_elem_atan(A@address,
                                              C@address,
                                              8L)
           },
           stop("type not recognized")
    )
    
    if(inplace){
        return(invisible(C))
    }else{
        return(C)    
    }
}


# GPU Element-Wise Hyperbolic Tan
vclVecElemHypTan <- function(A, inplace = FALSE){
    
    type <- typeof(A)
    
    if(inplace){
        C <- A
    }else{
        C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_tanh(A@address,
                                            C@address,
                                            6L)
           },
           double = {
               cpp_vclVector_elem_tanh(A@address,
                                             C@address,
                                             8L)
           },
           stop("type not recognized")
    )
    
    if(inplace){
        return(invisible(C))
    }else{
        return(C)    
    }
}


# GPU Element-Wise Natural Log
vclVecElemLog <- function(A){
    
    type <- typeof(A)
    
    C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_log(A@address,
                                           C@address,
                                           6L)
           },
           double = {
               cpp_vclVector_elem_log(A@address,
                                            C@address,
                                            8L)
           },
           stop("type not recognized")
    )
    return(C)
}


# GPU Element-Wise Log Base
vclVecElemLogBase <- function(A, base){
    
    type <- typeof(A)
    
    C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_log_base(A@address,
                                                C@address,
                                                base,
                                                6L)
           },
           double = {
               cpp_vclVector_elem_log_base(A@address,
                                                 C@address,
                                                 base,
                                                 8L)
           },
           stop("type not recognized")
           )
    return(C)
}


# GPU Element-Wise Base 10 Log
vclVecElemLog10 <- function(A){
    
    type <- typeof(A)
    
    C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_log10(A@address,
                                             C@address,
                                             6L)
           },
           double = {
               cpp_vclVector_elem_log10(A@address,
                                              C@address,
                                              8L)
           },
           stop("type not recognized")
    )
    return(C)
}


# GPU Element-Wise Exponential
vclVecElemExp <- function(A){
    
    type <- typeof(A)
    
    C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_exp(A@address,
                                            C@address,
                                            6L)
           },
           double = {
               cpp_vclVector_elem_exp(A@address,
                                             C@address,
                                             8L)
           },
           stop("type not recognized")
    )
    return(C)
}


# GPU Element-Wise Absolute Value
vclVecElemAbs <- function(A, inplace = FALSE){
    
    type <- typeof(A)
    
    if(inplace){
        C <- A
    }else{
        C <- vclVector(length=length(A), type=type, ctx_id = A@.context_index)   
    }
    
    switch(type,
           integer = {
               stop("integer not currently implemented")
           },
           float = {cpp_vclVector_elem_abs(A@address,
                                           C@address,
                                           6L)
           },
           double = {
               cpp_vclVector_elem_abs(A@address,
                                            C@address,
                                            8L)
           },
           stop("type not recognized")
    )
    return(C)
}

# vclVecElemAbs2 <- function(A){
# 
#     type <- typeof(A)
# 
#     switch(type,
#            integer = {
#                stop("integer not currently implemented")
#            },
#            float = {
#                file <- system.file("CL", "fabs.cl", package = "gpuR")
# 
#                if(!file_test("-f", file)){
#                    stop("kernel file does not exist")
#                }
#                kernel <- readChar(file, file.info(file)$size)
# 
#                cpp_vclVector_elem_abs2(A@address,
#                                            kernel,
#                                             A@.context_index - 1,
#                                            6L)
#            },
#            double = {
#                file <- system.file("CL", "dabs.cl", package = "gpuR")
# 
#                if(!file_test("-f", file)){
#                    stop("kernel file does not exist")
#                }
#                kernel <- readChar(file, file.info(file)$size)
# 
#                cpp_vclVector_elem_abs2(A@address,
#                                        kernel,
#                                        A@.context_index - 1,
#                                       8L)
#            },
#            stop("type not recognized")
#     )
#     return(invisible(A))
# }

# GPU Element-Wise Absolute Value
vclVecElemMaxAbs <- function(A){
    
    type <- typeof(A)
    
    out <- switch(type,
                  integer = {
                      stop("integer not currently implemented")
                  },
                  float = {cpp_vclVector_elem_max_abs(A@address,
                                                      6L)
                  },
                  double = {
                      cpp_vclVector_elem_max_abs(A@address,
                                                 8L)
                  },
                  stop("type not recognized")
    )
    return(out)
}

# GPU Vector maximum
vclVecMax <- function(A){
    
    type <- typeof(A)
    
    C <- switch(type,
                integer = {
                    stop("integer not currently implemented")
                },
                float = {cpp_vclVector_max(A@address, 6L)
                },
                double = {
                    cpp_vclVector_max(A@address, 8L)
                },
                stop("type not recognized")
    )
    return(C)
}


# GPU Vector minimum
vclVecMin <- function(A){
    
    type <- typeof(A)
    
    C <- switch(type,
                integer = {
                    stop("integer not currently implemented")
                },
                float = {cpp_vclVector_min(A@address, 6L)
                },
                double = {
                    cpp_vclVector_min(A@address, 8L)
                },
                stop("type not recognized")
    )
    return(C)
}


