eriak
=====

Riak client for Erlang

app: eriak

eriak
  - Top level module


eriak_pool
  - contains default as well as named pools
  - works as a factory
  - allows creation of mapreduce jobs via functions to add phases etc
  - Manages pools and connection check out/in
  - supports methods as new and get (includes multiget with configurable concurrency which depends on number of available connections) and delete (optionally with parameters/options)
  - initially only supports protobuf interface
  - possible to modify pool settings while running

  - use poolboy?

eriak_bucket
  - Interface for bucket properties
  - Base for object creation (factory function) ???

eriak_object - riak object
  - simple getter and setter methods/ same name to get and set,     xxx/1 is a get method,   xxx/2,3,4 is setter methods
  - resolvers (on get?)
  - methods to save/delete (optionally with props)
  - Uses connections behind the scenes
  - may contain siblings
  - one can check if it has been updated
  - automatically resolves identical siblings?
  - contains metadata dictionary in more intuitive form
  - eliminate support for links and link walking?
  - eliminate for riak search, at least from initial version

eriak_value - combination of value and metadata (contents in current client)
  - methods for getting indexes, metadata and values as well as content types
  - values are always updated on the object level
  - function to compare values (data, metadata, indexes, content type, encoding, charset)

eriak_search - functions for creating and running search queries

eriak_mapreduce - functions for building and running mapreduce queries



eriak.erl                                              [Methods for different types of requests and factories]

eriak_app.erl                                          [Pull initial config]
     |
eriak_pool_manager.erl                                 [Initiates pools and designation of defaults as well as pool lists]
     |
eriak_pool.erl                                         [Single pool managing connections. ]
      |
eriak_pb_connection.erl / eriak_http_connection.erl    [Manages connection logic. Similar to riakc_pb_socket]



Comments about Java Connection Pool
===================================

Our short-term requirements are:
- Have a pool of good and bad clients.
- A failed client will be moved from good to bad pool.
- Clients in bad pool are checked (ping) periodically and moved from bad pool to good pool.
- If the number of "good clients" decreases bellow a configurable level the call will wait for a time to a "good client" or throws an exception if timed-out.

On a long-term we'd like to have a dynamic number of RawClients configurable via ZooKeeper or any other mechanism.










