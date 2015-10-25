#!/usr/bin/env python
import click, re, sys, pdb
from neovim import attach

IP_ADDR = re.compile(r'^\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}(?:\:\d{1,5})?$')

@click.command(context_settings=dict(allow_extra_args=True))
@click.option('--address')
@click.option('--send', default=None)
@click.option('--expr', default=None)
@click.option('--tab/--no-tab', default=False)
@click.option('--silent/--no-silent', default=False)
@click.pass_context
def main(ctx, address, send, expr, tab, silent):
    if IP_ADDR.match(address):
        args = ('tcp',)
        kwargs = {'address': address}
    else:
        args = ('socket',)
        kwargs = {'path': address}
    try:
        nvim = attach(*args, **kwargs)
    except Exception as e:
        if not silent:
            print >> sys.stderr, e.message
        sys.exit(1)

    if send:
        nvim.input(send)
    elif expr:
        print nvim.eval(expr)
    else:
        files = ctx.args
        if not files:
            print >> sys.stderr, 'Need at least one file to edit'
            sys.exit(1)
        cmd = 'tabedit' if tab else 'edit'
        for f in files:
            nvim.command('{0} {1}'.format(cmd, f))

if __name__ == '__main__':
    main()
