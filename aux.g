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
