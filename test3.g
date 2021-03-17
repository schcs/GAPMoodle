Q1 := function()

    local mat, eigs, wronganswerrange, p1, p2, title, qtext, answers, defgrade, 
            penalty, single, shuffle, tags, qrec;

    repeat 
        mat := RandomMatrix( 2, [-2..2 ]);
        eigs := Eigenvalues( Rationals, mat );
    until Length( eigs ) = 2 and IsInt( eigs[1] ) and IsInt( eigs[2] );

    wronganswerrange := [-10..10];
    p1 := Position( wronganswerrange, eigs[1] );
    p2 := Position( wronganswerrange, eigs[2] );
    
    if p1 <> fail then Remove( wronganswerrange, p1 ); fi;
    if p2 <> fail then Remove( wronganswerrange, p2 ); fi;

    title := "Escolha os autovalores";
    
    qtext := Concatenation( "Quais são autovalores da matriz \\(", LaTeXObj( mat ), 
                            "\\)?" );
    answers := [[eigs[1], 50], 
                [eigs[2], 50], 
                [Random( wronganswerrange ), -50 ],
                [Random( wronganswerrange ), -50 ]];

    defgrade := 1;
    penalty := "0.1";
    single := false;
    shuffle := 1;
    tags := [ "Q1", "Csaba" ];

    qrec := rec( 
            title := title,
            type := "multi",
            qtext := qtext,
            answers := answers,
            defgrade := defgrade,
            penalty := penalty,
            single := single, 
            shuffle := shuffle, 
            tags := tags );

    return qrec;
end;

Q2 := function()

    local mat, eigs, wronganswerrange, goodmats, badmats, allmats, matrixlist, X, numbercorrect, marks, title, qtext, answers, defgrade, 
            penalty, single, shuffle, tags, qrec;

    goodmats:= [];
    badmats:=[];
 
    repeat
        repeat 
            mat := RandomSymmetricMatrix( 3, [ 1..6 ]);
            eigs := Eigenvalues( Rationals, mat );
        until Length( eigs ) = 3 and IsInt( eigs[1] ) and 
                IsInt( eigs[2] ) and IsInt( eigs[2] );

    	if eigs[1] > 0 and 
            eigs[2] > 0 and eigs[3] > 0 and 
            Length( Set(goodmats) ) < 3 then 
                Add( goodmats, mat ); 
        elif Length( Set(badmats) ) < 3 then 
            Add( badmats, mat ); 
        fi;

    until Length( Set(goodmats) ) = 3 and Length( Set(badmats) ) = 3;
	
    allmats := Set(goodmats);
    UniteSet( allmats, Set( badmats ));

    numbercorrect:=1;

    X := Random(goodmats);
    matrixlist := [ [X,1] ];
    RemoveSet(allmats, X);

    X := Random(allmats);
    if X in goodmats then 
        numbercorrect := numbercorrect+1;
        Add( matrixlist, [X,1]);
    else 
        Add( matrixlist, [X,2]); 
    fi;
    RemoveSet(allmats, X);
    
    X:=Random(allmats);
    if X in goodmats then 
        numbercorrect := numbercorrect+1;
        Add( matrixlist, [X,1]);
    else 
        Add( matrixlist, [X,2]); 
    fi;
    RemoveSet(allmats, X);

    X:=Random(allmats);
    if X in goodmats then 
        numbercorrect:=numbercorrect+1;
        Add( matrixlist, [X,1]);
    else 
        Add( matrixlist, [X,2]); 
    fi;
    RemoveSet(allmats, X);

    marks:=[0,0];
    if numbercorrect = 1 then marks[1]:="100"; marks[2]:="-33.333";fi;
    if numbercorrect = 2 then marks[1]:="50"; marks[2]:="-50";fi;
    if numbercorrect = 3 then marks[1]:="33.333"; marks[2]:="-100";fi;

    title := "Matriz eh IP?";
    
    qtext :=  "Marque as matrizes \\(A\\) com respeito a quais a forma bilinear \\(\\langle -,-\\rangle_A\\) é um produto interno";
    answers := [ [ Concatenation("\\(",LaTeXObj(matrixlist[1][1]),"\\)"), 
                                marks[matrixlist[1][2]]], 
                 [ Concatenation("\\(",LaTeXObj(matrixlist[2][1]),"\\)"), 
                                marks[matrixlist[2][2]]], 
                 [ Concatenation("\\(",LaTeXObj(matrixlist[3][1]),"\\)"), 
                                marks[matrixlist[3][2]]],
                 [ Concatenation("\\(",LaTeXObj(matrixlist[3][1]),"\\)"), 
                                marks[matrixlist[4][2]]]];

    defgrade := 1;
    penalty := "0.1";
    single := false;
    shuffle := 1;
    tags := [ "Q2", "John" ];

    qrec := rec( 
            title := title,
            type := "multi", 
            qtext := qtext,
            answers := answers,
            defgrade := defgrade,
            penalty := penalty,
            single := single, 
            shuffle := shuffle,
            tags := tags );

    return qrec;

end;

Q3 := function()

    local qrec, subquestions,mats; 

    repeat 
        mats := List( [1..4], x->RandomMatrix( 2, [-5..5] ));
        mats := Set( mats );
    until Length( mats ) = 4;

    subquestions := List( [1..4], x->[ Concatenation("\\(",LaTeXObj(mats[x]),"\\)"), 
                                  Determinant( mats[x] )]);

    qrec := rec(
            title := "Combine com determinante",
            type := "matching",
            qtext := "Combine a matriz com seu determinante", 
            subquestions := subquestions, 
            defgrade := "1", 
            penalty := "0.1", 
            shuffle := "1", 
            tags := [ "Q3", "match" ]); 
    
    return qrec;
end;