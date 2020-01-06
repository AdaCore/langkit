<html>
    <head>
        <title>Index - ${report.title}</title>
        <link rel="StyleSheet" type="text/css" href="coverage.css" />
    </head>
    <body>
        <h1>Index - ${report.title}</h1>

        % for group in report.groups.values():
            <h2>${group.label}</h2>
            <table>
                <tr>
                    <th>Source file</th>
                    <th>Summary</th>
                </tr>
                % for _, f in sorted(group.files.iteritems()):
                    <tr>
                        <td><a href="${f.html_file}">${f.name}</a></td>
                        <td>
                            <table class="summary"><tr>
                                % for state in report.SUMMARY_STATES:
                                    % if f.summary[state]:
                                        <td class="${report.state_name(state)}"
                                            width="${f.summary[state] * 100}%">
                                        </td>
                                    % endif
                                % endfor
                            </tr></table>
                        </td>
                    </tr>
                % endfor
            </table>
        % endfor
    </body>
</html>
