# Creates random square matrix. The first two complements define the dimension and the # range of entries. The optional third argument is a boolean function which the output
# must satisfy. 

RandomMatrix := function( args... )
    local dim, range, func, mat;

    dim := args[1]; range := args[2];
    if Length( args ) >= 3 then 
        func := args[3];
    elif Length( args ) < 3 then
        func := x->true;
    fi;

    repeat 
        mat := List( [1..dim], x-> List( [1..dim], x->Random( range )));
    until func( mat );

    return mat;
end;

# Same as the previous but creates random symmetric matrix. 

RandomSymmetricMatrix := function( args... )

    local dim, range, func, mat, i, j; 

    dim := args[1]; range := args[2];
    if Length( args ) >= 3 then 
        func := args[3];
    elif Length( args ) < 3 then
        func := x->true;
    fi;

    repeat 

        mat := List( [1..dim], x->[] );
        for i in [1..dim] do
            for j in [1..dim] do
                if i <= j then 
                    mat[i,j] := Random( range );
                else
                    mat[i,j] := mat[j,i];
                fi;
            od;
        od;
    until func( mat );
    return mat;
end;

# Returns matrix is LaTeX format. May not be necessary because there of LaTeXObj.

RandomVector := function( length, range, args... )
    local prop, vec; 

    if Length( args ) > 0 then 
        prop := args[1];
    else
        prop := x->true;
    fi;

    repeat
        vec := List( [1..length], x->Random( range ));
    until prop( vec );

    return vec;
end;



LaTeXMatrix := function( mat )

    local str, nr, nc, i, j;

    str := "\\(\\begin{pmatrix}\n";
    nr := NrRows( mat );
    nc := NrCols( mat );

    for i in [1..nr] do
        for j in [1..nc] do
            str := Concatenation( str, String( mat[i][j] ));
            if j = nc and i <> nr then 
                str := Concatenation( str, " \\\\\n " );
            elif j <> nc then 
                str := Concatenation( str, " \& " );
            fi;
        od;
    od;

    str := Concatenation( str, "\n\\end{pmatrix}\\)" );

    return str;
end;

#checks if symmetric matrix is positive definite

IsPositiveDefinite := function( mat )
    local d, i;

    if TransposedMat( mat ) <> mat then 
        Error( "matrix must be symmetric" );
    fi;

    d := NrRows( mat );
    for i in [1..d] do
        if Determinant( mat{[1..i]}{[1..i]}) <= 0 then 
            return false;
        fi;
    od;

    return true;
end;

#checks if matrix has at least k integer eigenvalues

HaskIntegerEigenvalues := function( mat, k )

    return Length( Filtered( Eigenvalues( Rationals, mat ), IsInt )) >= k;
end;

InstallMethod( LaTeXObj, [IsString], x->x );

InstallMethod( LaTeXObj, [ IsList ], 
    function( vec )
        local str;
        str := String( vec );
        str := ReplacedString( str, "[", "(" );
        str := ReplacedString( str, "]", ")" );
        return str;
    end );

        

InnerProductByMatrix := function( v1, v2, A )
    return v1*A*v2;
end;

CosAngle := function( v1, v2, A )
    
    return InnerProductByMatrix( v1, v2, A )/Sqrt(InnerProductByMatrix( v1, v1, A )*
            InnerProductByMatrix( v2, v2, A ));
end;

#projection of v2 to v1
OrthogonalProjection := function( v1, v2, A )

    return InnerProductByMatrix( v1, v2, A )/InnerProductByMatrix( v1, v1, A )*v1;
end;


OrthGS := function( vecs, A )
    local newvecs, vec, i, j;

    newvecs := [vecs[1]];
    for i in [2..Length( vecs )] do
        vec := vecs[i];
        for j in [1..Length( newvecs )] do
            vec := vec - OrthogonalProjection( newvecs[j], vecs[i], A );
        od;
        Add( newvecs, vec );
    od;

    return newvecs;
end;

AdjointImage := function( T, v, A )
    local d, V, e, beta, GramMat;

    d := Length( v );
    V := RowSpace( Rationals, d );
    e := Basis( V );
    beta := List( [1..d], x -> InnerProductByMatrix( T*e[x], v, A ));

    GramMat := List( [1..d], x->List( [1..d], y->InnerProductByMatrix( e[x], e[y], A )));

    return GramMat^-1*beta;
end;