## vim: filetype=makopython

<%def name="accessor_body(field)">

        <% c_accessor = '_{}'.format(field.accessor_basename.lower) %>

        % if field.type.is_ast_node and not field.arguments:
        result = self._eval_astnode_field(${c_accessor})

        % else:
        ## Create C values for arguments
        % if any(arg.type.conversion_requires_context \
                 for arg in field.arguments):
        _context = self.unit.context._c_value
        % endif
        % for arg in field.arguments:
        unwrapped_${arg.name.lower} = ${pyapi.unwrap_value(arg.name.lower,
                                                           arg.public_type,
                                                           '_context')}
        % endfor

        ## Evaluate the property
        <%
            # Expression to create a holder for the C result
            c_result_constructor = '{}()'.format(
                pyapi.c_type(field.public_type))

            # Expression for the C value for field evaluation
            eval_args = [c_result_constructor, c_accessor] + [
                pyapi.extract_c_value(
                    'unwrapped_{}'.format(arg.name.lower),
                    arg.type,
                    for_arg=True,
                )
                for arg in field.arguments
            ]
        %>
        ## Evaluate the C value for field evaluation, and then the Python
        ## wrapper.
        c_result = self._eval_field(${', '.join(eval_args)})
        result = ${pyapi.wrap_value('c_result', field.public_type)}
        % endif

</%def>

<%def name="subclass_decls(cls)">
    <%
        # Parent class for "cls", or None if "cls" is actually the root AST
        # node (if we called .base on it, it would return ASTNodeType).
        parent_cls = cls.base if T.root_node != cls else None

        # Python expression that yield a tuple that contains the names for all
        # fields that "cls" inherits.
        parent_fields = (
            '{}._field_names'.format(pyapi.type_public_name(parent_cls))
            if parent_cls else '()'
        )
    %>

    % for field in cls.fields_with_accessors():
    <%
        arg_list = ['self']
        for a in field.arguments:
            arg = '{}: {}'.format(a.name.lower, a.type.mypy_type_hint)
            if a.default_value is not None:
                arg += ' = {}'.format(a.default_value.render_python_constant())
            arg_list.append(arg)
    %>
    % if not field.arguments:
    @property
    % endif
    def ${field.api_name.lower}(
        ${', '.join(arg_list)}
    ) -> ${field.type.mypy_type_hint}:
        ${py_doc(field, 8)}
        ${accessor_body(field)}
        return result
    % endfor

    _field_names = ${parent_fields} + (
        % for field in cls.get_parse_fields( \
            predicate=lambda f: not f.abstract and not f.null, \
            include_inherited=False \
        ):
        "${field.api_name.lower}",
        % endfor
    )

    % if not cls.abstract:
    _kind_name = ${repr(pyapi.type_public_name(cls))}
    % endif

    % if cls.is_list_type:
    is_list_type = True

    ## __iter__ and __getitem__ refinements for more precise list element types
    def __iter__(
        self
    ) -> Iterator[${cls.element_type.mypy_type_hint}]:
        return super().__iter__()  # type: ignore

    def __getitem__(
        self,
        index: int
    ) -> ${cls.element_type.mypy_type_hint}:
        return super().__getitem__(index)  # type: ignore
    % endif
</%def>

<%def name="decl(cls)">

class ${pyapi.type_public_name(cls)}(${pyapi.type_public_name(cls.base)}):
    ${py_doc(cls, 4)}
    __slots__ : Tuple[str, ...] = ()
${subclass_decls(cls)}

</%def>
