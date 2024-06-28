var createError = require('http-errors');
var express = require('express');
var path = require('path');
var cookieParser = require('cookie-parser');
var logger = require('morgan');
var DataStore = require('./convert/datastore');

var dotenv = require('dotenv');
dotenv.config();

var indexRouter = require('./routes/index');
var eventsRouter = require('./routes/events');
var statisticsRouter = require('./routes/statistics');
var pricingRouter = require('./routes/pricing');
var apiRouter = require('./routes/api');

var app = express();

var port = process.env.PORT || 3000;
app.set('port', port);

// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'pug');

app.use(logger('dev'));
app.use(express.json());
app.use(express.urlencoded({extended: false}));
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

var sdkConfig = {sdkKey: '10035569/10034190'}; // [ConvertSDK]

var dataStore = new DataStore();
sdkConfig.dataStore = dataStore; // [ConvertSDK] optional

var ConvertSDK = require('@convertcom/js-sdk').default; // [ConvertSDK]
var sdkInstance = new ConvertSDK(sdkConfig); // [ConvertSDK]

var ConvertContext = require('./middleware/convertcontext'); // [ConvertSDK]
app.use(ConvertContext(sdkInstance, dataStore)); // [ConvertSDK]

app.use('/', indexRouter);
app.use('/events', eventsRouter);
app.use('/statistics', statisticsRouter);
app.use('/pricing', pricingRouter);
app.use('/api', apiRouter);

// catch 404 and forward to error handler
app.use(function (req, res, next) {
  next(createError(404));
});

// error handler
app.use(function (err, req, res, next) {
  // set locals, only providing error in development
  res.locals.message = err.message;
  res.locals.error = req.app.get('env') === 'development' ? err : {};

  // render the error page
  res.status(err.status || 500);
  res.render('error');
});

console.log('NodeJS Demo available at http://localhost:' + port);

module.exports = app;
