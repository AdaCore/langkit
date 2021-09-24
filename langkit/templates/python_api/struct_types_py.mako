## vim: filetype=makopython

<%def name="ctype_fields(cls)"> [
    % for field in cls.get_fields():
        ('${field.name.lower}',
         ## At this point in the binding, no array type has been emitted
         ## yet, so use a generic pointer: we will do the conversion later
         ## for users.
         % if field.type.is_array_type:
             ctypes.c_void_p
         % else:
            ${pyapi.c_type(field.type)}
         % endif
         ),
    % endfor
] </%def>

<%def name="base_decls()">

class _BaseStruct:
    """
    Mixin for Ada struct wrappers.
    """

    # Subclasses will override this to a subclass of ctypes.Structure
    _c_type: ClassVar[ctypes.Structure]

    def __getitem__(self, key: int) -> Any:
        if not isinstance(key, int):
            raise TypeError(
               'Tuples items are indexed by integers, not {}'.format(type(key))
            )

        fields = self._c_type._fields_
        if 0 <= key < len(fields):
            field_name, _ = fields[key]
            return getattr(self, field_name)
        else:
            raise IndexError('There is no {}th field'.format(key))

    def __repr__(self) -> str:
        field_names = [name for name, _ in self._c_type._fields_]
        return '<{} {}>'.format(
            type(self).__name__,
            ' '.join('{}={}'.format(name, getattr(self, name))
                      for name in field_names)
        )

    @property
    def as_tuple(self) -> tuple:
        return tuple(getattr(self, f) for f, _ in self._c_type._fields_)

    def __eq__(self, other: Any) -> bool:
        return (isinstance(other, type(self)) and
                self.as_tuple == other.as_tuple)

    def __ne__(self, other: Any) -> bool:
        return not (self == other)

    def __hash__(self) -> int:
        return hash(self.as_tuple)


</%def>

<%def name="decl(cls)">

<% public_name = pyapi.type_public_name(cls) %>

class ${public_name}(_BaseStruct):
    ${py_doc(cls, 4)}

    <% field_names = [f.name.lower for f in cls.get_fields()] %>

    __slots__ = (${', '.join(repr('_' + f) for f in field_names)}${(
        ', ' if len(field_names) == 1 else '')})

    def __init__(
        self,
        % for f in cls.get_fields():
        ${f.name.lower}: ${f.type.mypy_type_hint},
        % endfor
    ):
        % for f in field_names:
        self._${f} = ${f}
        % endfor
        % if not field_names:
        pass
        % endif

    % for field in cls.get_fields():

    @property
    def ${field.name.lower}(self) -> ${field.type.mypy_type_hint}:
        ${py_doc(field, 8)}
        return self._${field.name.lower}
    % endfor

    class _c_type(ctypes.Structure):
        _fields_ = ${ctype_fields(cls)}

    class _Holder:
        def __init__(self, c_value):
            self.c_value = c_value

        def clear(self):
            self.c_value = None

        def __del__(self):
            % if cls.is_refcounted:
            ${public_name}._dec_ref(self.c_value)
            % endif
            self.clear()

    @classmethod
    def _wrap(cls, c_value):
        return cls(
            % for field in cls.get_fields():
            <%
                fld = 'c_value.{}'.format(field.name.lower)
                if field.type.is_array_type:
                    fld = 'ctypes.cast({}, {})'.format(
                        fld,
                        pyapi.c_type(field.type)
                    )
                copy = pyapi.wrap_value(fld, field.type,
                                        from_field_access=True)
            %>${copy},
            % endfor
        )

    @classmethod
    def _unwrap(cls, value, context=None):
        if not isinstance(value, cls):
            _raise_type_error(cls.__name__, value)

        % for f in cls.get_fields():
        <% field_name = f.name.lower %>
        ${field_name} = ${pyapi.unwrap_value(
            'value.{}'.format(field_name),
            f.type,
            'context'
        )}
        % endfor

        result = cls._Holder(cls._c_type(
            % for f in cls.get_fields():
            <%
                field_name = f.name.lower
                field_value = pyapi.extract_c_value(field_name, f.type)
                if f.type.is_array_type:
                    field_value = ('ctypes.cast({}, ctypes.c_void_p)'
                                   .format(field_value))
            %>
            ${field_name}=${field_value},
            % endfor
        ))

        ## Our Python holders for ref-counted C values own a refcounting share,
        ## so we must inc-ref all fields so that the created structure owns a
        ## share.
        % if cls.is_refcounted:
        cls._inc_ref(result.c_value)
        % endif

        return result

    % if cls.is_refcounted:
    _c_ptr_type = ctypes.POINTER(_c_type)
    _inc_ref = staticmethod(_import_func('${cls.c_inc_ref(capi)}',
                            [_c_ptr_type], None))
    _dec_ref = staticmethod(_import_func('${cls.c_dec_ref(capi)}',
                            [_c_ptr_type], None))
    % endif

</%def>

<%def name="mypy_decl(cls)">

class ${pyapi.type_public_name(cls)}:
    ${py_doc(cls, 4)}

    def __init__(
        self,
        % for f in cls.get_fields():
        ${f.name.lower}: ${f.type.mypy_type_hint},
        % endfor
    ) -> None: ...

    % for f in cls.get_fields():
    @property
    def ${f.name.lower}(self) -> ${f.type.mypy_type_hint}:
        ${py_doc(f, 8, or_pass=True)}
    % endfor

</%def>
