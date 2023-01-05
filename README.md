# zotonic_mod_disrupt


# Controllers

## `controller_disrupt`

The disrupt controller allows you to disrupt the working of another controller.
It can add latency, or let it error. This makes it possible to allow one to 
find out what happens on the client side when a certaint dispatch errors, or
suddenly has a high latency.

Config options:

 - `delegate`: The name of the controller which should handle the real request.
 - `distupt_type`: The type of disruption to apply. Either `throw` or `latency`.
 - `disrupt_probability`: Then probability wether or not to apply the distruption.

...

# Settings 

 - `latency_mean`: The latency mean to add to dispatches. Default: **300**
 - `latency_variance`: The latency variance to add to dispatches. Default: **400**
 
 
