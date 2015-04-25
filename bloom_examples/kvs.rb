require "reliableBroadcast.rb"
// FIXME: shouldn't need to redundantly include, right?
require "reliableDelivery.rb"

module KVSProtocol {
  state {
    interface input, kvput, [:client, :key] => [:reqid, :value]
    interface input, kvdel, [:key] => [:reqid]
    interface input, kvget, [:reqid] => [:key]
    interface output, kvget_response, [:reqid] => [:key, :value]
  }
}

module BasicKVS {
  include KVSProtocol

  state :foo {
    table :kvstate, [:key] => [:value]
  }

  bloom :mutate {
    kvstate <+ kvput {|s| [s.key, s.value]}
    kvstate <- (kvput * kvstate) on (kvput.key == kvstate.key) { |p, s|
        s
    }
  }

  bloom :get {
    kvget_response <~ (kvget * kvstate) on (kvget.key == kvstate.key) { |g, t|
      [g.reqid, t.key, t.value]
    }
  }
}

module ReplicatedKVS {
  import BasicKVS => kvs
  include KVSProtocol
  import ReliableBcast => bcast

  bloom :glue {
    kvs->kvput <= kvput
    kvs->kvget <= kvget
    kvget_response <~ kvs->kvget_response
  }

  bloom {
    bcast->pipe_in <= kvput{|k| [k.client, k.reqid, k]}
  }
}


include ReplicatedKVS

