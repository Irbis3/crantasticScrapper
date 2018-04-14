setwd( paste0( dropbox_folder, "/rosalind/sign" ) )
n <- scan( "rosalind_sign.txt" )
nums <- c(-n:-1, 1:n)
perms <- do.call( expand.grid, rep( list( nums ), n ) )
perms <- perms[ apply( abs(perms), 1, function(x) {
  length( unique( x ) ) == n
} ), ]

if( file.exists( "out2.txt" ) ) {
  file.remove( "out2.txt" )
}

outFile <- file( "out2.txt", 'a' )

cat( nrow(perms), "\n", file=outFile )
invisible( apply( perms, 1, function(x) { cat(x, "\n", file=outFile) } ) )

close( outFile )