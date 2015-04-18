
module KVSProtocol {
  state {
    interface input, kvput, [:client, :key] => [:reqid: int, :value]
    interface input, kvdel, [:key] => [:reqid: int]
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
        //[s.key, s.value]
        s
    }
  }


  bloom :get {
    //temp :getj <= (kvget * kvstate).pairs(:key => :key)
    //kvget_response <= getj { |g, t|
    kvget_response <~ (kvget * kvstate) on (kvget.key == kvstate.key) { |g, t|
      [g.reqid, t.key, t.value]
    }
  }
}


include BasicKVS

