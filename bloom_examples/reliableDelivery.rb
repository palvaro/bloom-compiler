module DeliveryProto {
	state {
		input send, [:sender, :recipient, :id, :msg]
		output rcv, [:recipient, :sender, :id, :msg]
		output sent, [:sender, :recipient, :id, :msg]
	}
}

module BestEffortDelivery {
	include DeliveryProto
	state {
		channel deliv, [:recipient, :sender, :id, :msg]
	}
	bloom {
		deliv <~ send{|s| [s.recipient, s.sender, s.id, s.msg]}
		rcv <= deliv
		sent <= send
	}
}

module ReliableDelivery {
	include DeliveryProto
	import BestEffortDelivery => bed

	state {
		table send_buf, [:sender, :recipient, :id, :msg]
		table ack_buf, [:sender, :recipient, :id, :msg]
        channel ack, [:sender, :recipient, :id]
	}

	bloom {
		send_buf <= send
		rcv <~ bed->rcv
		bed->send <= send_buf.notin(ack_buf)
		ack <~ bed->rcv{|r| [r.sender, r.recipient, r.id]}
		ack_buf <= (ack * send_buf) on (ack.id == send_buf.id) {|a, s| [a.sender, a.recipient, a.id, s.msg]}
		send_buf <- ack_buf
	}
}