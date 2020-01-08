from __future__ import absolute_import, division, print_function

from langkit.passes import GlobalPass

templates = {}


def sf(strn):
    """
    Real string splicing in cpython.

    Usage:

    >>> a = 12
    >>> b = 15
    >>> r = range(5)
    >>> sf("${a - b} : ${', '.join(str(c) for c in r)}")

    :param basestring strn: The string to format, containing regular python
                            expressions in ${} blocks.
    :rtype: unicode
    """

    from mako.template import Template
    import inspect

    frame = inspect.stack()[1][0]
    if not templates.get(strn):
        t = templates[strn] = Template(
            "\n".join(map(str.strip, strn.splitlines()))
        )
    else:
        t = templates[strn]

    return t.render(**dict(frame.f_locals, **frame.f_globals))


def emit_rule(rule):
    from langkit.parsers import (
        _Transform, node_name, _Row, Opt, List, Or, _Token, NoBacktrack,
        _Extract, DontSkip, Skip, Null, Parser, resolve, Defer, Predicate
    )
    if isinstance(rule, _Transform):
        inner = emit_rule(rule.parser)
        if len(inner) > 50:
            return "{}($i$hl{}$d$hl)".format(node_name(rule.typ), inner)
        else:
            return "{}({})".format(node_name(rule.typ), inner)
    elif isinstance(rule, _Row):
        return " $sl".join(emit_rule(r) for r in rule.parsers)
    elif isinstance(rule, Opt):
        return (
            "?({})".format(emit_rule(rule.parser))
            if isinstance(rule.parser, _Row)
            else "?{}".format(emit_rule(rule.parser))
        )
    elif isinstance(rule, _Extract):
        return emit_rule(rule.parser)
    elif isinstance(rule, List):
        sep_str = ", {}".format(emit_rule(rule.sep)) if rule.sep else ""
        return "list{}({}{})".format(
            '+' if rule.empty_valid else '*', emit_rule(rule.parser), sep_str
        )
    elif isinstance(rule, Null):
        return "null({})".format(
            emit_rule(rule.type) if isinstance(rule.type, Parser)
            else node_name(rule.type)
        )
    elif isinstance(rule, Or):
        body = ' | '.join(emit_rule(r) for r in rule.parsers)
        if len(body) > 50:
            return "or($i$hl| {}$d$hl)".format(
                '$hl| '.join(emit_rule(r) for r in rule.parsers)
            )
        else:
            return "or({})".format(body)
    elif isinstance(rule, DontSkip):
        return "{}.dont_skip({})".format(
            emit_rule(rule.subparser),
            " ".join((emit_rule(resolve(d)) for d in rule.dontskip_parsers))
        )
    elif isinstance(rule, Skip):
        return "skip({})".format(node_name(rule.dest_node))
    elif isinstance(rule, _Token):
        if isinstance(rule._val, basestring):
            return '"{}"'.format(rule._val)
        else:
            return "@{}{}".format(
                rule._val.name.camel,
                '("{}")'.format(rule.match_text)
                if rule.match_text else ""
            )
    elif isinstance(rule, NoBacktrack):
        return "/"
    elif isinstance(rule, Defer):
        return str(rule)
    elif isinstance(rule, Predicate):
        return "{} |> when({})".format(
            emit_rule(rule.parser), rule.property_name
        )
    else:
        raise NotImplementedError()


def var_name(var_expr, default="_"):
    return (
        var_expr.source_name.lower if var_expr.source_name else default
    )


def is_simple_expr(expr):
    from langkit.expressions import FieldAccess, Literal, AbstractVariable
    return isinstance(expr, (FieldAccess, Literal, AbstractVariable))


def emit_indent_expr(expr, **ctx):
    strn = emit_expr(expr, **ctx)
    if len(strn) > 40:
        return "$i$hl{}$d$hl".format(strn)
    else:
        return strn


def emit_paren_expr(expr, **ctx):
    from langkit.expressions import Let
    if is_simple_expr(expr):
        strn = emit_expr(expr, **ctx)
        return emit_paren(strn) if len(strn) > 40 else strn
    elif isinstance(expr, Let):
        return emit_expr(expr, **ctx)
    else:
        return emit_paren(emit_expr(expr, **ctx))


def emit_paren(strn):
    if len(strn) > 40:
        return "($i$hl{}$d$hl)".format(strn)
    else:
        return "({})".format(strn)


def emit_nl(strn):
    if len(strn) > 40:
        return "$i$hl{}$d$hl".format(strn)
    else:
        return " {} ".format(strn)


def emit_expr(expr, **ctx):
    from langkit.expressions import (
        Literal, Let, FieldAccess, AbstractVariable, SelfVariable,
        EntityVariable, LogicTrue, LogicFalse, unsugar, Map, All, Any,
        GetSymbol, Match, Eq, BinaryBooleanOperator, Then, OrderingTest,
        Quantifier, If, IsNull, Cast, DynamicVariable, IsA, Not, SymbolLiteral,
        No, Cond, New, CollectionSingleton, Concat, EnumLiteral, EnvGet,
        ArrayLiteral, Arithmetic
    )

    then_underscore_var = ctx.get('then_underscore_var')
    overload_coll_name = ctx.get('overload_coll_name')

    def is_a(*names):
        return any(expr.__class__.__name__ == n for n in names)

    def emit_lambda(expr, vars):
        vars_str = ", ".join(var_name(var) for var in vars)
        return "({}) => {}".format(vars_str, ee(expr))
        del vars_str

    def emit_method_call(receiver, name, args=[]):
        if not args:
            return "{}.{}".format(receiver, name)
        else:
            return "{}.{}{}".format(
                receiver, name, emit_paren(", ".join(args))
            )

    def ee(expr, **extra_ctx):
        full_ctx = dict(ctx, **extra_ctx)
        return emit_expr(expr, **full_ctx)

    def ee_pexpr(expr, **extra_ctx):
        full_ctx = dict(ctx, **extra_ctx)
        return emit_paren_expr(expr, **full_ctx)

    expr = unsugar(expr)

    if isinstance(expr, Literal):
        return str(expr.literal).lower()
    elif isinstance(expr, SymbolLiteral):
        return '"{}"'.format(expr.name)
    elif isinstance(expr, IsA):
        return "{} is_a {}".format(
            ee_pexpr(expr.expr),
            ", ".join(type_name(t) for t in expr.astnodes)
        )
    elif isinstance(expr, LogicTrue):
        return "ltrue"
    elif isinstance(expr, LogicFalse):
        return "lfalse"
    elif isinstance(expr, Let):

        if len(expr.vars) == 0:
            return ee(expr.expr)

        vars_defs = "".join([
            "{} = {}$hl".format(
                var_name(var), ee(abs_expr)
            ) for var, abs_expr in zip(expr.vars, expr.var_exprs)
        ])

        return "let$i$hl{}$din$i$hl{}$hl$dend".format(
            vars_defs, ee(expr.expr)
        )
    elif isinstance(expr, Map):
        op_name = expr.kind
        print(op_name)
        args = []
        vars = [expr.element_var]
        if expr.requires_index:
            vars.append(expr.index_var)
        if op_name in ["map", "mapcat"]:
            args.append(emit_lambda(expr.expr, vars))
        elif op_name == "filter":
            args.append(emit_lambda(expr.filter_expr, vars))
        elif op_name == "filter_map":
            args.append(emit_lambda(expr.expr, vars))
            args.append(emit_lambda(expr.filter_expr, vars))
        elif op_name == "take_while":
            args.append(emit_lambda(expr.take_while_expr, vars))

        if overload_coll_name:
            op_name = overload_coll_name
            del ctx['overload_coll_name']

        coll = ee(expr.collection)

        return emit_method_call(coll, op_name, args)

    elif isinstance(expr, Quantifier):
        return emit_method_call(
            ee(expr.collection),
            expr.kind,
            [emit_lambda(expr.expr, [expr.element_var])]
        )

    elif isinstance(expr, If):
        res = "if {} then {} else {}".format(
            ee_pexpr(expr.cond), ee_pexpr(expr._then), ee_pexpr(expr.else_then)
        )

        return res

    elif isinstance(expr, Cond):
        branches = expr.branches
        parts = ["if {} then {}".format(
            ee_pexpr(branches[0][0]), ee_pexpr(branches[0][1])
        )]
        parts += ["elif {} then {}".format(ee_pexpr(cond), ee_pexpr(then))
                  for cond, then in expr.branches[1:]]
        parts.append("else {}".format(ee_pexpr(expr.else_expr)))

        return "$hl".join(parts)

    elif isinstance(expr, IsNull):
        return "{}.is_null".format(ee(expr.expr))

    elif isinstance(expr, Cast):
        return "{}.to{}[{}]".format(
            ee(expr.expr),
            "!" if expr.do_raise else "",
            type_name(expr.dest_type)
        )

    elif isinstance(expr, All):
        return ee(expr.equation_array, overload_coll_name="logic_all")
    elif isinstance(expr, Any):
        return ee(expr.equation_array, overload_coll_name="logic_any")

    elif isinstance(expr, Match):
        return sf("""
        match ${ee(expr.matched_expr)} do$i$hl
        % for typ, var, e in expr.matchers:
        % if typ:
        case ${var_name(var)} : ${type_name(typ)} => ${ee(e)}$hl
        % else:
        case ${var_name(var)} => ${ee(e)}$hl
        % endif
        % endfor
        $d$hlend
        """)

    elif isinstance(expr, Eq):
        return "{} == {}".format(ee(expr.lhs), ee(expr.rhs))

    elif isinstance(expr, BinaryBooleanOperator):
        return "{} {} {}".format(
            emit_paren_expr(expr.lhs),
            expr.kind, emit_paren_expr(expr.rhs)
        )

    elif isinstance(expr, Not):
        return "not {}".format(emit_paren_expr(expr.expr))

    elif isinstance(expr, Then):
        if expr.var_expr.source_name is None:
            assert expr.underscore_then
            return "{}?{}".format(
                ee(expr.expr),
                ee(expr.then_expr, then_underscore_var=expr.var_expr)
            )

        res = "{} then {}".format(
            ee_pexpr(expr.expr),
            emit_paren("{} => {}".format(
                var_name(expr.var_expr),
                ee(expr.then_expr),
            )),
        )
        if expr.default_val:
            res += " else {}".format(ee_pexpr(expr.default_val))

        return res

    elif isinstance(expr, OrderingTest):
        return "{} {} {}".format(
            ee_pexpr(expr.lhs),
            expr.OPERATOR_IMAGE[expr.operator],
            ee_pexpr(expr.rhs)
        )

    elif isinstance(expr, GetSymbol):
        return "{}.symbol".format(ee(expr.node_expr))
    elif is_a("as_entity", "as_bare_entity", "at_or_raise", "children",
              "env_parent", "rebindings_parent", "parents", "parent", "root",
              "append_rebinding", "concat_rebindings", "env_node",
              "rebindings_new_env", "rebindings_old_env", "get_value",
              "solve", "is_referenced_from", "env_group", "length"):
        exprs = expr.sub_expressions
        return emit_method_call(ee(exprs[0]), type(expr).__name__,
                                map(ee, exprs[1:]))

    elif isinstance(expr, EnumLiteral):
        return expr.value.dsl_name

    elif isinstance(expr, Arithmetic):
        return "{} {} {}".format(ee_pexpr(expr.l), expr.op, ee_pexpr(expr.r))

    elif isinstance(expr, EnvGet):
        args = [ee(expr.symbol)]
        if expr.sequential_from:
            args.append("from={}".format(ee(expr.sequential_from)))
        if expr.only_first:
            args.append("only_first={}".format(ee(expr.only_first)))
        return emit_method_call(ee(expr.env), "get", args)

    elif is_a("bind"):
        return "bind{}in$i$hl{}$d$hlend".format(
            emit_nl("{}={}".format(ee(expr.expr_0), ee(expr.expr_1))),
            ee(expr.expr_2)
        )
    elif is_a("at"):
        # Recognize find
        if (isinstance(expr.expr_0, Map) and expr.expr_0.kind == 'filter' and
                ee(expr.expr_1) == "0"):
            return ee(expr.expr_0, overload_coll_name="find")

        return "{}?[{}]".format(ee(expr.expr_0), ee(expr.expr_1))
    elif isinstance(expr, FieldAccess):
        args = []
        if expr.arguments:
            for arg in expr.arguments.args:
                args.append(ee(arg))
            for name, arg in expr.arguments.kwargs.items():
                args.append("{}={}".format(name, ee(arg)))

        return emit_method_call(ee(expr.receiver), expr.field, args)

    elif isinstance(expr, Concat):
        return "{} & {}".format(ee_pexpr(expr.array_1), ee_pexpr(expr.array_2))

    elif isinstance(expr, EntityVariable):
        return "entity"
    elif isinstance(expr, SelfVariable):
        return "self"
    elif isinstance(expr, DynamicVariable):
        return expr.argument_name.lower
    elif isinstance(expr, AbstractVariable):
        if then_underscore_var:
            if id(then_underscore_var) == id(expr):
                return ""
        return var_name(expr)
    elif isinstance(expr, No):
        return "null"
    elif isinstance(expr, CollectionSingleton):
        return "[{}]".format(ee(expr.expr))
    elif isinstance(expr, New):
        return "new {}{}".format(
            type_name(expr.struct_type),
            emit_paren(", ".join(
                "{}={}".format(k, ee(v)) for k, v in expr.field_values.items()
            ))
        )
    elif isinstance(expr, ArrayLiteral):
        return "[{}]".format(", ".join(ee(el) for el in expr.elements))

    else:
        # raise NotImplementedError(type(expr))
        return repr(expr)


def emit_prop(prop):
    quals = ""
    if prop.is_public:
        quals += "public "

    if prop.abstract:
        quals += "abstract "

    if prop.memoized:
        quals += "memoized "

    args = ", ".join("{} : {}{}".format(
        arg.name.lower, type_name(arg.type),
        "= {}".format(arg.default_value) if arg.default_value else ""
    ) for arg in prop.natural_arguments)

    res = "$hl{}fun {} ({}): {}".format(
        quals, prop.original_name.lower, args, type_name(prop.type)
    )

    if prop.expr:
        res += " = {}".format(emit_indent_expr(prop.expr))

    return res


def emit_field(field):
    from langkit.compiled_types import BaseField, Field
    if isinstance(field, BaseField):
        return "{}field {} : {}".format(
            "parse " if isinstance(field, Field) else "",
            field._indexing_name, type_name(field.type)
        )
    else:
        raise NotImplementedError()


def type_name(type):
    from langkit.compiled_types import ASTNodeType, resolve_type

    type = resolve_type(type)
    if isinstance(type, ASTNodeType):
        if type.is_list_type:
            return "ASTList[{}]".format(type_name(type.element_type))
        else:
            return type.raw_name.camel
    elif type.is_array_type:
        return "Array[{}]".format(type_name(type.element_type))
    elif type.is_entity_type:
        return "Entity[{}]".format(type_name(type.element_type))
    elif type.is_struct_type:
        return type.api_name.camel
    else:
        return type.name.camel


def emit_node_type(node_type):
    base = node_type.base
    parse_fields = node_type.get_fields(
        include_inherited=False
    )
    properties = node_type.get_properties(include_inherited=False)
    enum_qual = (
        "qualifier " if node_type.is_bool_node
        else "enum " if node_type.is_enum_node else ""
    )

    if base and base.is_enum_node:
        return ""

    return sf("""
    % if base:
    ${enum_qual}class ${type_name(node_type)} : ${type_name(base)} is$i$hl
    % else:
    ${enum_qual}class ${type_name(node_type)} is$i$hl
    % endif
    % for field in parse_fields:
    ${emit_field(field)}$hl
    % endfor
    % if enum_qual == "enum ":
        case $i${(", $sl".join(alt.name.camel
                               for alt in node_type.alternatives))}$d$hl
    % endif
    % for prop in properties:
    % if not prop.is_internal:
    ${emit_prop(prop)}$hl
    % endif
    % endfor
    $d
    end$hl$hl
    """.strip())

    del base, parse_fields, enum_qual, properties


def unparse_lang(ctx):
    emitter = ctx.emitter
    ctx.emitter = None

    template = """
    grammar ${ctx.short_name}_grammar is$i$hl
    % for name, rule in ctx.grammar.rules.items():
    % if not rule.is_dont_skip_parser:
    ${name} <- ${emit_rule(rule)}$hl
    % endif
    % endfor
    $d$hl
    end$hl

    % for node_type in ctx.astnode_types:
    ${emit_node_type(node_type)}

    % endfor
    """

    from time import time
    t = time()
    lang_def = pp(sf(template))
    print(lang_def)
    t2 = time()
    print(t2 - t)
    with open("{}_lang_def.lkt".format(ctx.short_name), 'w') as f:
        f.write(lang_def)

    ctx.emitter = emitter


def create():
    return GlobalPass('Unparse language definition', unparse_lang)


def pp(strn, indent_step=4, line_size=80):
    from cStringIO import StringIO
    import re
    buf = re.split("(\$hl|\$sl|\$i|\$d)", strn)
    file_str = StringIO()
    indent_level = 0
    current_line = ""

    def write_line():
        file_str.write(current_line.rstrip() + "\n")

    for i in range(len(buf)):
        el = buf[i].replace("\n", "")
        next_el = buf[i] if i < len(buf) else ""
        if el in ['$hl', '$sl']:
            if el == '$hl' or len(current_line) + len(next_el) > line_size:
                write_line()
                current_line = " " * indent_level
        elif el == '$i':
            indent_level += indent_step
        elif el == '$d':
            indent_level -= indent_step
            # Reset current line if there is only whitespace on it
            if current_line.strip() == "":
                current_line = " " * indent_level
        else:
            current_line += el
    if current_line:
        write_line()
    return file_str.getvalue()
