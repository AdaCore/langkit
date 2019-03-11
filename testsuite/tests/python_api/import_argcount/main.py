from __future__ import absolute_import, division, print_function

print('main.py: Running...')


import libfoolang


for args, kwargs in [
    ([], {}),
    (['hello'], {}),
    ([], {'hello': 'world'}),
]:
    print('Trying to call with {} and {}...'.format(args, kwargs))
    try:
        libfoolang._get_last_exception(*args, **kwargs)
    except TypeError as exc:
        print('   Got a TypeError exception: {}'.format(exc))
    else:
        print('   Success')


print('main.py: Done.')
