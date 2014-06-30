# CHANGELOG

## 0.6.5 (June 30, 2014)

* Refactor schema-table


## 0.6.4 (May 26, 2014)

* Support to execute dialyzer


## 0.6.3 (May 8, 2014)

* Bump leo_rpc to 0.8.3
* Improved
    * To correctly synchronization of tables
    * Modifued deletion of column tables
    * Took over sync-fun from savanna-agent to savanna-commons


## 0.6.2 (Mar 29, 2014)

* Bump leo_commons to 1.0.1


## 0.6.1 (Mar 4, 2014)

* Optimized the library
    * Refactor - Optimized whole functions
    * Implemented that an end datetime of a sampling adjusts a step


## 0.6.0 (Feb 27, 2014)

* Optimized the library
    * Included folsom-sup into the savanna-commons's sup in order to easily embed applications
    * Implemented `svc_operate_behaviour` in order to be able to add other metrics


## 0.4.4 (Feb 25, 2014)

* Improved the function of trim-and-notify
* Implemented to be able to set proc-expirration-time
* Include folsom-sup into the savanna-commons-sup


## 0.4.3 (Feb 20, 2014)

* Improved the performance


## 0.4.2 (Feb 18, 2014)

* Fixed bugs and improvements
    * Correctly handle return of a creating metric
    * Change unit of window


## 0.4.1 (Feb 17, 2014)

* Staggered a notification to one of metric-servers in order to restrain the processing


## 0.4.0 (Feb 14, 2014)

* Improved all histogram-metrics
    * Support histogram-metrics:[uniform, slide, exdec]
* Implemented that a metric-proc is able to terminate when it was not notified from clients
    * Also a terminated proc is able to re-generate metrics


## 0.2.2 (Feb 12, 2014)

* Improve histogram-metrics


## 0.2.1 (Feb 7, 2014)

* Add a behaviour of notifier in order to collect metrics and statistics


## 0.2.0 (Feb 5, 2014)

* Initial release
