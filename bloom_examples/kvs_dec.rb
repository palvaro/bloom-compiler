module Eggs do
    state {
        table foo, [:key]
        table bar, [:key]
    }
end

module TastyNest {
    include Eggs
    bloom {
        foo <= bar
    }
}

//include TastyNest

import TastyNest => nest1
import TastyNest => nest2
