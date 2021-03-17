# Creates an answer in Moodle format for multiple choice questions. 
# The input consists of either two arguments or a list of two elements. 
# The first input is the answer, the second is its value.

MoodleAnswer := function( args... )
    local answer, value, text;

    if Length( args ) = 1 then 
        answer := args[1][1]; value := args[1][2];
    else 
        answer := args[1]; value := args[2];
    fi;

    text := Concatenation( "<answer fraction=\"",
            String( value ), "\" format=\"html\">\n", 
            "<text><![CDATA[<p>",
            String( answer ),
            "</p>]]><\/text>\n<\/answer>\n" );

    return text;
end;

# Creates a subquestion in Moodle format for matching questions. 
# The input consists of either two arguments or a list of two elements. 
# The first input is the answer, the second is its value.

MoodleSubQuestion := function( args... )
    local question, answer, text;

    if Length( args ) = 1 then 
        question := args[1][1]; answer := args[1][2];
    else 
        question := args[1]; answer := args[2];
    fi;

    text := Concatenation( "<subquestion format=\"html\">\n", 
            "<text><![CDATA[<p>",
            String( question ),
            "</p>]]><\/text>\n",
            "<answer><text>", String( answer ), "<\/text><\/answer>\n", 
            "<\/subquestion>\n" );

    return text;
end;


# The function for creating a multiple choice question.
# It takes a record as input with the following fields:
#            title                      The title of the question 
#            type                       The type of the questions. Currently implemented
#                                       "multi" and "matching"
#                                        
#            qtext                      The text of the question
#            answers                    The list of possible answers answers. Each   #                                       answer is a list of two components, the first
#                                       being a string representing the answer, while 
#                                       the second is the value of the answer. 
#                                       It is recommended that the second component be 
#                                       also given as a string, for example "33.3333".
#            defgrade                   The default grade.
#            penalty                    Penalty (not sure what it does)
#            single                     true or false whether or not there is only one 
#                                       correct answer. 
#            shuffle                    "1" if we want answers to be permuted 
#            tags                       list of tags                        


MoodleMultipleChoiceQuestion := function( qrec )

    local text, ans, tag;

    text := Concatenation( "<question type=\"multichoice\">\n<name format=\"html\">\n",
            "<text><![CDATA[", qrec.title, "]]><\/text>\n",
            "</name>\n", 
            "<questiontext format=\"html\">\n",
            "<text><![CDATA[<p>", qrec.qtext, "<\/p>]]><\/text>\n",
            "<\/questiontext>\n",
            "<defaultgrade>", String( qrec.defgrade ), "<\/defaultgrade>\n", 
            "<generalfeedback format=\"html\"><text\/><\/generalfeedback>\n",
            "<penalty>", qrec.penalty, "<\/penalty>\n",
            "<hidden>0<\/hidden>\n",
            "<single>", String( qrec.single ), "<\/single>\n", 
            "<shuffleanswers>", String( qrec.shuffle ), "<\/shuffleanswers>\n", 
            "<answernumbering>abc<\/answernumbering>\n" );

    for ans in qrec.answers do
        text := Concatenation( text, MoodleAnswer( ans ));
    od;

    if IsBound( qrec.tags ) and Length( qrec.tags ) > 0 then 
        text := Concatenation( text, "<tags>\n" );
        for tag in qrec.tags do
            text := Concatenation( text, "<tag>\n<text>", tag, "<\/text>\n<\/tag>\n" );
        od;
        text := Concatenation( text, "<\/tags>\n" );
    fi;

    text := Concatenation( text, "<\/question>\n" );

    return text;
end;

# The function for creating a mathcing questions.
# It takes a record as input with the following fields:
#            title                      The title of the question 
#            qtext                      The text of the question
#            subquestions               The list of subquestions. Each   #                                       subquestion is a list of two components. 
#                                       It is recommended that the second component be 
#                                       also given as a string, for example "33.3333".
#            defgrade                   The default grade.
#            penalty                    Penalty (not sure what it does)
#            shuffle                    "1" if we want answers to be permuted 
#            tags                       list of tags                        

MoodleMatchingQuestion := function( qrec )

    local text, ans, tag;

    text := Concatenation( "<question type=\"matching\">\n<name format=\"html\">\n",
            "<text><![CDATA[", qrec.title, "]]><\/text>\n",
            "</name>\n", 
            "<questiontext format=\"html\">\n",
            "<text><![CDATA[<p>", qrec.qtext, "<\/p>]]><\/text>\n",
            "<\/questiontext>\n",
            "<defaultgrade>", String( qrec.defgrade ), "<\/defaultgrade>\n", 
            "<generalfeedback format=\"html\"><text\/><\/generalfeedback>\n",
            "<penalty>", qrec.penalty, "<\/penalty>\n",
            "<hidden>0<\/hidden>\n",
            "<shuffleanswers>", String( qrec.shuffle ), "<\/shuffleanswers>\n" );
    
    for ans in qrec.answers do
        text := Concatenation( text, MoodleSubQuestion( ans ));
    od;

    if IsBound( qrec.tags ) and Length( qrec.tags ) > 0 then 
        text := Concatenation( text, "<tags>\n" );
        for tag in qrec.tags do
            text := Concatenation( text, "<tag>\n<text>", tag, "<\/text>\n<\/tag>\n" );
        od;
        text := Concatenation( text, "<\/tags>\n" );
    fi;

    text := Concatenation( text, "<\/question>\n" );

    return text;
end;


# Function to create Moodle Questionnaire. The input is a record with fields
#       category       name of category under which the questions will appear on Moodle 
#       questions      the list of questions    

MoodleQuestionnaire := function( qqrec )

    local str, q;

    str := Concatenation( "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n", 
            "<!-- This file was generated by a GAP program -->\n\n",
            "<quiz>\n\n", 
            "<question type=\"category\">\n<category>\n",
            "<text>$course$/top/", qqrec.category, "<\/text>\n",
            "<\/category>\n<\/question>\n\n" );

    for q in qqrec.questions do
        if q.type = "multi" then
            str := Concatenation( str, MoodleMultipleChoiceQuestion( q ));
        elif q.type = "matching" then 
            str := Concatenation( str, MoodleMatchingQuestion( q ));
        else
            Error( "Question type not implemented" );
        fi;
    od;

    str := Concatenation( str, "\n<\/quiz>\n" );

    return str;
end;