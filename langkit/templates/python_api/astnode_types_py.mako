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
                pyapi.extract_c_value('unwrapped_{}'.format(arg.name.lower),
                                      arg.type)
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
        arg_list = ['self'] + [
            (a.name.lower
             if a.default_value is None else
             '{}={}'.format(a.name.lower,
                            a.default_value.render_python_constant()))
            for a in field.arguments
        ]
    %>
    % if not field.arguments:
    @property
    % endif
    def ${field.api_name.lower}(${', '.join(arg_list)}):
        ${py_doc(field, 8,
                 argtypes=[(a.name.lower, a.type) for a in field.arguments],
                 rtype=field.type)}
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
    % endif
</%def>

<%def name="mypy_field_decls(cls, or_pass=True)">
    <%
        fields = cls.fields_with_accessors()
        is_list = cls.is_list_type
    %>

    ## Type hints for field accessors
    % for field in fields:
    <%
        arg_list = ['self'] + [
            (
                '{}: {}'.format(a.name.lower, a.type.mypy_type_hint)
                if a.default_value is None else
                '{}: {} = {}'.format(
                    a.name.lower,
                    a.type.mypy_type_hint,
                    a.default_value.render_python_constant()
                )
            )
            for a in field.arguments
        ]
    %>
    % if not field.arguments:
    @property
    % endif
    def ${field.api_name.lower}(
        ${', '.join(arg_list)}
    ) -> ${field.type.mypy_type_hint}: ...
    % endfor

    ## __iter__ and __getitem__ refinements for more precise list element types
    % if is_list:
    def __iter__(self) -> Iterator[${cls.element_type.mypy_type_hint}]: ...
    def __getitem__(self,
                    index: int) -> ${cls.element_type.mypy_type_hint}: ...
    % endif

    ## "pass" when needed to keep the class declaration syntax valid
    % if or_pass and not (fields or is_list):
    pass
    % endif

</%def>

<%def name="mypy_decl(cls)">
class ${pyapi.type_public_name(cls)}(${pyapi.type_public_name(cls.base)}):
${mypy_field_decls(cls)}

</%def>

<%def name="decl(cls)">

class ${pyapi.type_public_name(cls)}(${pyapi.type_public_name(cls.base)}):
    ${py_doc(cls, 4)}
    __slots__ = []
${subclass_decls(cls)}

</%def>
