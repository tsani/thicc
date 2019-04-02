THICC
=====

The Haskell Implemented Cluster Controller.

Workflow for starting a new service
-----------------------------------

* Boot the required number of backend containers for the service.
* Boot a load balancer for the service
* Allocate a hostname for the balancer in the DNS
* Create and register the new LB config.
* Service is up.

Workflow for scaling a service
------------------------------

* Boot / shutdown the appropriate number of backends for the service.
* Update LB config.

Workflow for dynamic scaling
----------------------------

* Monitor resource utilization (CPU) within a service.
* If utilization passes a threshold, issue a scaling command.
* Use hysteresis.
