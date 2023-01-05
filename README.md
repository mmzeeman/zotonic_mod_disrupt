# zotonic_mod_disrupt

This module can be used to test the resilince of your Zotonic system. It has a controller
which can be used to crash or add artificial latency to requests. It uses the 
Erlang havoc module, which is a Chaos Monkey style system.

<img width="1270" alt="Screen Shot 2023-01-05 at 20 32 09" src="https://user-images.githubusercontent.com/1024972/210864830-8fdb5539-88b6-4515-85d9-6175bc9400a2.png">

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
 
 
