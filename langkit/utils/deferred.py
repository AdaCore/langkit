from __future__ import annotations

from typing import Callable, Generic, TypeVar


# Type for the reference owners
_Owner = TypeVar("_Owner")

# Type for references themselves
_Reference = TypeVar("_Reference")

# Callback that returns the reference
_Resolver = Callable[[], _Reference]

# Callback that assigns the reference to the owning type
_Applier = Callable[[_Owner, _Reference], None]


class DeferredEntityResolver(Generic[_Owner, _Reference]):
    """
    Helper to defer the resolution of entities.

    When creating instances for various entities (types, members, ...), the
    instances corresponding to dependent entities (members or generic
    interfaces for types, implemented methods for type members, ...) may not
    be available.

    In order to allow these references nevertheless, a common solution is to
    have deferred references: when creating an instance for entity E1 (e.g. a
    node type) that needs to refer to entity E2 (e.g. a generic interface
    that E1 implements):

    * One first calls the ``add`` method, passing E1 as well as a callback that
      returns E2.

    * Two possible paths from there:

      * When the instance for E2 is not available yet, the request (E1 plus the
        callback) is registered in a waiting queue. Once the compilation
        process states that these entities (generic interfaces) are available
        (i.e. calling the ``resolve`` method), all callbacks are evaluated, and
        the resulting entities are assigned: the node E1 will get its
        refererence to the generic interface E2.

      * When the instance for E2 is available (``resolve`` was already called),
        the callback is evaluated directly and E1 is assigned its deference to
        E2 (no need to defer anything in this case).
    """

    def __init__(self, applier: _Applier) -> None:
        """
        :param applier: Callback to invoke on owner/resolved entity couples
            after resolution to attach the resolved entity to the owner.
        """
        self._applier = applier

        # Queue for the references to resolve. Set to None once the resolution
        # is trigered, so that new requests are evaluated right away.
        self._queue: list[tuple[_Owner, _Resolver]] | None = []

    def add(self, owner: _Owner, resolver: _Resolver) -> None:
        """
        Defer the resolution of the entity for the given owner (if the
        ``resolve`` method was not called yet) or resolve it right away (if
        ``resolve`` was already called).
        """
        if self._queue is None:
            self._applier(owner, resolver())
        else:
            self._queue.append((owner, resolver))

    def resolve(self) -> None:
        """
        Trigger the resolution for all deferred references.

        This must be called exactly once durnig the compilation process: once
        all entities can be resolved.
        """
        assert self._queue is not None

        # First disable the deferring mechanism and only then resolve entities,
        # so that we do not defer additional resolutions in the process.
        saved_queue = self._queue
        self._queue = None

        for owner, resolver in saved_queue:
            self._applier(owner, resolver())
