# CHANGELOG

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
