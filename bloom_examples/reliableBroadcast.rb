require "reliableDelivery.rb"


module MembershipProto {
	state {
		table node, [:node, :neighbor]
	}
}

module BcastProto {
	state {
		input pipe_in, [:node, :id, :payload: record]
		output pipe_out, [:node, :id, :payload: record]
		output pipe_sent, [:node, :id, :payload: record]
	}
}

module BasicBcast  {
	include MembershipProto
	include BcastProto
	import ReliableDelivery => rd

	bloom {
		rd->send <= (pipe_in * node) on (pipe_in.node == node.node) {|p, n|
			[p.node, n.neighbor, p.id, p.payload]
		}
		pipe_out <~ rd->rcv{|d| [d.recipient, d.id, d.msg]}
	}
}

module ReliableBcast {
	include BcastProto
	import BasicBcast => basic

	state {
		scratch missing_ack, [:node, :neighbor, :id]
		scratch send_done, [:node, :neighbor, :id]
		scratch icare, [:node, :neighbor, :id]
		scratch cleanup, [:node, :id, :payload]
	}

	bloom :sender {
		// make the "on" optional!
		basic->pipe_in <= pipe_in
		pipe_out <~ basic->pipe_out
	}
}

module SymmetricRB {
	include ReliableBcast

	bloom {
		//send_buf <+ basic->pipe_out.notin(cleanup)
		basic->pipe_in <= basic->pipe_out
	}
}


//include SymmetricRB
include ReliableBcast