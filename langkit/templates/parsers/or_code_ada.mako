## vim: filetype=makoada

${parser.pos_var} := No_Token_Index;
${parser.res_var} := ${parser.type.storage_nullexpr};

## Keep track of both:
##
## * The state of diagnostics before running this or() parser.
${parser.render_set_mark()}
${parser.last_fail_var} := Parser.Last_Fail;

## * The state of diagnostics for the most relevant or() branch subparser. In
##   case all subparsers have failed, we want to "return" this chain of
##   diagnostics.
${parser.branch_diag_mark_var} := Parser.Last_Diag;
${parser.branch_last_fail_var} := Parser.Last_Fail;

% for subparser in parser.parsers:
    ## Before running this branch subparser, first restore diagnostics to their
    ## pre-or() point.
    ${parser.render_rollback()}
    Parser.Last_Fail := ${parser.last_fail_var};

    ${subparser.generate_code()}
    if ${subparser.pos_var} /= No_Token_Index then
        ## This branch subparser was successful: consider this or() parser done
        ${parser.pos_var} := ${subparser.pos_var};
        ${parser.res_var} := ${subparser.res_var};
        goto ${exit_label};
    end if;

    ## The subparser failed: if it went further than previous subparsers,
    ## update the diagnostics we will use if all branches fail.
    if Parser.Last_Fail.Pos >= ${parser.branch_last_fail_var}.Pos then
       ${parser.branch_diag_mark_var} := Parser.Last_Diag;
       ${parser.branch_last_fail_var} := Parser.Last_Fail;
    end if;
% endfor

## All subparsers failed: use diagnostics from the most advanced subparser
Parser.Last_Diag := ${parser.branch_diag_mark_var};
Parser.Last_Fail := ${parser.branch_last_fail_var};
<<${exit_label}>>
