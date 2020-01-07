<html>
    <head>
        <title>${src_file.name} - ${report.title}</title>
        <link rel="StyleSheet" type="text/css" href="coverage.css" />
        <script type="text/javascript">
            function toggle(ids) {
                ids.forEach(function toggle_single(id) {
                    var style = document.getElementById(id).style;
                    if (style.display === "none") {
                        style.display = "table-row";
                    } else {
                        style.display = "none";
                    }
                });
            }
        </script>
    </head>
    <body>
        <h1>${src_file.name} - ${report.title}</h1>

        <p><a href="index.html">Back to the index</a></p>

        <table class="code">
            % for line in src_file.lines:
                <%
                    annotation_ids = ['l{}_{}'.format(line.lineno, i)
                                      for i, _ in enumerate(line.annotations)]
                    id_array = '[{}]'.format(', '.join(
                        repr(id) for id in annotation_ids
                    ))
                %>
                <tr class="${state_name(line.state)}"
                    onclick="toggle(${id_array})">
                    <td class="lineno"><pre>${line.lineno}</pre></td>
                    <td class="state"><pre>${line.state}</pre></td>
                    <td class="src"><pre>${escape(line.content)}</pre></td>
                </tr>

                 % for id, a in zip(annotation_ids, line.annotations):
                     <tr id="${id}" style="display: none;">
                         <td></td>
                         <td></td>
                         <td>${a.kind}: ${escape(a.message)}</td>
                     </tr>
                 % endfor
            % endfor
        </table>

        <p><a href="index.html">Back to the index</a></p>
    </body>
</html>
