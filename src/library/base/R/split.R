split <-
    function( x, f )
    UseMethod( "split" )

split.default <-
    function( x, f )
    .Internal( split( x, as.factor( f ) ) )

split.data.frame <-
    function( x, f )
{
    lapply( split( 1:nrow( x ), f ), function( ind ) x[ ind, , drop = FALSE ] )
}
