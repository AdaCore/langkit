## vim: filetype=makoada

--  Start row_code

${pos} := ${pos_name};

## This is the main body of the row, which is the concatenation of the code for
## each row part.
${body}

pragma Warnings (Off, "referenced");
<<${exit_label}_0>>
pragma Warnings (On, "referenced");

--  End row_code
