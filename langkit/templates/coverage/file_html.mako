<html>
    <head>
        <title>${src_file.name} - ${report.title}</title>
        <link rel="StyleSheet" type="text/css" href="coverage.css" />
    </head>
    <body>
        <h1>${src_file.name} - ${report.title}</h1>

        <p><a href="index.html">Back to the index</a></p>

        <table class="code">
            % for line in src_file.lines:
                <tr class="${state_name(line.state)}">
                    <td class="lineno"><pre>${line.lineno}</pre></td>
                    <td class="state"><pre>${line.state}</pre></td>
                    <td class="src"><pre>${escape(line.content)}</pre></td>
                </tr>

                ## For now, do not display annotations. Always displaying them
                ## is invasive (adds a lot of noise), and displaying them on
                ## demand would require some HTML/JavaScript scripting.
                % if False:
                    % for a in line.annotations:
                        <tr>
                            <td></td>
                            <td></td>
                            <td>${a.kind}: ${escape(a.message)}</td>
                        </tr>
                    % endfor
                % endif
            % endfor
        </table>

        <p><a href="index.html">Back to the index</a></p>
    </body>
</html>
