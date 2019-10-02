Internal design
---------------

The cfg application provides a schema driven tree database with long
lived session transactions for updating multiple entries atomically.



Schema
------

The Schema used is yang compatible opening the door to future use of
yang modules as a source of the schema.

Today the database schema is created by the managed system using the
APIs provided here.

The managed system must define a tree of nodes and leafs using the
functions in the cfg module:

* cfg:container/4 - create a container node with children

* cfg:list/4 - create a node to act as parent to a list of other trees or leafs

* cfg:leaf_list - create a node to hold a list of single typed values
                  e.g numbers or strings

* cfg:leaf - create a node to hole a single typed value

The managed system must then install the schema by calling
cfg:load_schema/1 with a list of functions that will create the
top level nodes of the tree.



Configuration transactions
==========================

Creating a transaction
----------------------

A configuration transaction is created typically when the user
switches to configuration mode in the CLI.

The new transaction creates a full copy of the configuration
database in a transaction local ets table. cfg_db:copy_to_ets() will
grab a copy of the database from whichever storage backend is used.

Setting a value
---------------

Setting a value at a path will update the ets copy of the database and
append a {set, Path, Value} item in a list that can be applied
to the master database on commit.

This design allows changes to other parts of the tree to be made by
other transactions while a transaction is ongoing. The alternative
would be to completely replace the master database on commit, wiping
out other changes (or more likely making configuration mode available
to a single user at a time (exclusive mode).

Setting a value also requires that each level in the path is checked
against the schema

List items
----------

List items appear in the database as multiple #cfg{} records of
node_type = list at the same level, with the list key in the path
stored as a tuple of potentially compound values that must also exist
as leafs in the database.

The list #cfg{} value field is a handy place to store the names of the
parameters that go into the key.

The leaf values that make up the list key must be set in the
database. This is examtly in line with the yang specifications.

Example set parameters during a transaction:

    [{set ["interface", {"eth0"}, "name"], "eth0"},
     {set ["interface", {"eth0"}, "speed"], "1GbE"}].

These two set parameters will result in the following database entries:

#cfg{node_type = container, path = ["interface"], name = "interface"}
#cfg{node_type = list, path = ["interface", {"eth0"}], value = ["name"]}
#cfg{node_type = leaf, path = ["interface", {"eth0"}, "name"], name = "name", value = "eth0"}
#cfg{node_type = leaf, path = ["interface", {"eth0"}, "speed"], name = "speed", value = "1GbE"}

The additional nodes will be created if they don't already exist

List items at the same level as other entries
---------------------------------------------

In the above example the "interface" container exists at the same level
as the list item. This opens the door to CLI elements like:

set interface <TAB>

Possible Completions:
  global - Global interface settings

Possible list items:
  eth0
  eth1

I'm not sure whether this is supported by Yang?

Multiple list items in the schema at the same level is prevented by
the schema loader

Namespaces
----------

In yang all nodes exist in an XML style namespace. This is supported,
but optional here. Any schema nodes without a namespace are put in a
global table with namespace set to the atom 'default'.

Database backends
-----------------

The system is designed to support different storage backends
The storage engine is configurable when calling cfg:init/2

Storage backends must provide set of functions to mirror the mnesia API:

init() - Called once at startup to allow the backend to create tables / init schema etc.

transaction(Fun) - Run a transaction against the DB where Fun will perform any updates

read(Key) - read entry at Key

write(#cfg{} = Record) - write Record to the config table

Fun can use cfg_db:read/1, cfg_db:write/1 which will be re-directed to
the backend specific functions.

