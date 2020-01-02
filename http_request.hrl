-include("http_uri.hrl").

-record(http_request, {
          method = <<>> :: bitstring(),
          uri = #http_uri{} :: httpuri(),
          version = <<>> :: bitstring()
         }).
