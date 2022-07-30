mgmtd
=====

A self contained system for management of configuration and operational data in an Erlang program

It can be used standalone, or combined with ecli to allow cli menu access

Features (ok, wish list)
- Configuration and operational data driven by either json schema or Yang
- Embedded schema driven configuration database
- Option to provide external database interface
- Callback based interface to host system to retrieve operational data
- Subscription interface to recieve configuration changes
- Transaction based configuration changes - multiple items changed, then commit
- Override configuration items with environment variables at startup
- Automatic / programmable schema upgrades

Configuration
===

mgmtd itself can be configured in a number of ways. This config can be provided
in sys.config, via environment variables, or dynamically.

Available items:

- Configuration database directory
- Env variable override prefix


Components
===



mgmtd_schema
===========

Takes both JSON schema and Yang files in any combination and provides
a common api to query the schema and validate data against the schema.

The data model is based on Yang. JSON schema is squeezed into Yang
concepts where there is a conflict.

Build
-----

    $ rebar3 compile
