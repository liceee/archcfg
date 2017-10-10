
        (function(window) {
        /**
 Interface for component. Every component should be derived from this interface
 @constructor
 */
function ComponentInterface() {
    /** Create the component
     * @param {Object} config of the application
     */
    this.create = function(config) {};

    /** Clear any data the component may create */
    this.cleanup = function() {};

    /** Destroy component */
    this.destroy = function() {};

    /* Notify component about options
    * @param {Object} options to use*/
    this.options = function(options) {};
}


/**
 Application context. Manages components (registration, creation, destroy, clean). Can work as another component
 @param {String} name of the context
 @extents ComponentInterface
 @constructor
 */
function DCMContext(name) {
    ComponentInterface.call(this);
    var configuration = {};
    var componentsMap = {};
    var logDump = [];

    function getComponent(name) {
        var component = componentsMap[name];
        if (!component) {
            throw "Unknwown Component with name: "+name;
        }
        return component;
    }

    function checkCircularReference(component) {
        if (component.progress) {
            throw "Circular reference detected on element: "+component.name;
        }
        component.progress = true;
    }

    function executeCreate(component, initializedMap) {
        try {
            if (typeof component.loader == "function") {
                component.loader = new component.loader();
            }
            component.loader.create(configuration);
            initializedMap[component.name] = true;
        } catch (e) {
            throw "Error during component load: "+e;
        }
    }

    function createDependencies(component, initializedMap) {
        for (var i = 0; i < component.dependencies.length; i++) {
            var dependentComponent = getComponent(component.dependencies[i]);
            createComponent(dependentComponent, initializedMap);
        }
    }

    function createComponent(component, initializedMap) {
        if (initializedMap[component.name]) {
            return;
        }

        try {
            checkCircularReference(component);
            createDependencies(component, initializedMap);
            executeCreate(component, initializedMap);
        } catch (e) {
            throw e+"\nCannot load component: "+component.name;
        }

        component.progress = false;
    }

    function destroyChildren(component, destroyedMap) {
        var componentName = component.name;
        for (var childName in componentsMap) {
            var childComponent = getComponent(childName);
            if (childComponent.dependencies.indexOf(componentName) > -1 && !destroyedMap[componentName]) {
                destroyComponent(childComponent, destroyedMap);
            }
        }
    }

    function executeDestroy(component, destroyedMap) {
        try {
            component.loader.destroy();
        } catch (e) {
            //destruction errors. ignore
        }
        destroyedMap[component.name] = true;
    }

    function destroyComponent(component, destroyedMap) {
        if (destroyedMap[component.name]) {
            return;
        }

        destroyChildren(component, destroyedMap);
        executeDestroy(component, destroyedMap);
    }

    function cleanupUninitializedComponents(initializedMap) {
        for (var componentName in componentsMap) {
            if (!initializedMap[componentName]) {
                delete componentsMap[componentName];
            }
        }
    }

    /**
     Register component in application
     @param {String} name Unique name of current component
     @param {String[]} dependencies List of dependent components
     @param {ComponentInterface|Function} component Component object or constructor to create it.
     */
    this.addComponent = function(name, dependencies, component) {
        componentsMap[name] = {
            name: name,
            loader: component,
            dependencies: dependencies,
            progress: false
        }
    };

    function logStartupErrors(context) {
        if (logDump.length == 0) {
            return;
        }

        try {
            var logger = context.loggerFactory.getLogger("DCMContext");
            logger.error("Initialization errors:\n" + logDump.join("\n"));
        } catch (e) {
            //in case logging is broken anyway.
        }
        logDump = [];
    }

    /**
     Start Application
     @param {Object} config of the application
    */
    this.create = function(config) {
        configuration = config;
        var initializedMap = {};
        for (var name in componentsMap) {
            try {
                var component = getComponent(name);
                createComponent(component, initializedMap);
            } catch (e) {
                logDump.push(e.toString());
                //cannot initialize some component. good place for logging;
            }
        }
        cleanupUninitializedComponents(initializedMap);
        logStartupErrors(this);
    };

    /** Stop application*/
    this.destroy = function() {
        var destroyedMap = {};
        for (var name in componentsMap) {
            try {
                var component = getComponent(name);
                destroyComponent(component, destroyedMap);
            } catch (e) {
                //cannot destroy some component. ignore
            }
        }
    };

    /** Clean any data crated by components (Uninstall)*/
    this.cleanup = function() {
        for (var name in componentsMap) {
            try {
                var component = getComponent(name);
                component.loader.cleanup();
            } catch (e) {
                //error during data cleanup. good place for logging;
            }
        }
    };

    /** Notify components about new external options*/
    this.options = function(options) {
        for (var name in componentsMap) {
            try {
                var component = getComponent(name);
                component.loader.options(options);
            } catch (e) {
                //error during calling options. ignore
            }
        }
    };

    /** Return context name
     * @return String*/
    this.getName = function() {
        return name;
    }
}


/** Interface of the browser for DCM to interact with it.
 * @constructor */
function BrowserInterface() {
    // TODO: change the description below
    /** Returns if privacy mode is currently enabled. Use messages to get state changes.
     * @param {Function(ignoreAll, tabs)} callback to notify about results
     * @param {Boolean} callback.ignoreAll Indicates whether the data from all tabs should be discarded
     * @param {Array} callback.tabs The array of tab ids for which privacy mode is enabled.
     * Do not use when callback.ignoreAll is true */
    this.isPrivacyModeEnabled = function(callback) {
        throw "Method is not implemented";
    };

    /** Open Custom modal dialog in browser
     * @param {Object} config of the dialog
     * @param {String} config.title of the dialog
     * @param {String} config.text to display on dialog
     * @param {String} config.accept name of accept button
     * @param {String} config.reject name of reject button
     * @param {Function} [callback] The callback to be called when user closes dialog
     * @return {Boolean} If user accepted or discarded dialog question
     * */
    this.openDialog = function(config, callback) {
        return false;
    };

    this.openUrl = function(url) {
        throw "Method is not implemented";
    }
}

/** Interface of the Remote {@link BrowserInterface}
 * @extends BrowserInterface
 * @constructor*/
function RemoteBrowserInterface() {
    BrowserInterface.call(this);
    RemoteObjectSpec.call(this, {
        name: MESSAGING_DATA.REMOTE_BROWSER_COMPONENT,
        quietMethods: ["openUrl"],
        responseMethods: ["isPrivacyModeEnabled", "openDialog"]
    })
}

/** Communication Component which initialize communication mechanism in your context
 * @param {DCMContext} context of application
 * @constructor */
function CommunicationComponent(context) {
    ComponentInterface.call(this);

    this.create = function(config) {
        /** Communication related instances
         * @memberOf dcm
         * @memberOf engine */
        context.communication = {};

        /** Instance of client communication controller. Do not use it directly
         * @memberOf dcm
         * @type CommunicationController */
        context.communication.controller = new CommunicationController(context);
        context.communication.factory = new CommunicationFactory(context);
        context.communication.remote = new RemoteCallsController(context);
    }
}


/** Main class for sending/receiving messages.
 * @extends CommunicationControllerInterface
 * @constructor */
function CommunicationController() {
    CommunicationControllerInterface.call(this);
    ReceieversHandlerMixin.call(this);
    var transmitter;

    function notifyTransmitter(subject, topic, data) {
        if (transmitter) {
            var serialized = serializeData(data);
            transmitter.receive(subject, topic, serialized)
        }
    }

    function parseData(data) {
        return JSON.parse(data);
    }

    function serializeData(data) {
        if (data === undefined || data === null) {
            return JSON.stringify(null)
        } else if (data && data.getJson) {
            return data.getJson();
        } else {
            return JSON.stringify(data);
        }
    }

    this.send = function(subject, topic, data) {
        this.notifyReceivers(subject, topic, data);
    };

    this.transmit = function(subject, topic, data) {
        notifyTransmitter(subject, topic, data);
    };

    this.receive = function(subject, topic, data) {
        var parsed = parseData(data);
        this.notifyReceivers(subject, topic, parsed);
    };

    this.setTransmitter = function(theTransmitter) {
        transmitter = theTransmitter;
    };
}


/** Interface of the communication object
 * @constructor */
function CommunicationControllerInterface() {
    /** Send message to receivers
     * @param {String} subject of the message
     * @param {String} topic of the message
     * @param {Object|MessageData} data of the message*/
    this.send = function(subject, topic, data) {};

    /** Receive message from another communication controller
     * @param {String} subject of the message
     * @param {String} topic of the message
     * @param {String} data of the message*/
    this.receive = function(subject, topic, data) {};

    /** Transmit message to another context
     * @param {String} subject of the message
     * @param {String} topic of the message
     * @param {Object|MessageData} data of the message*/
    this.transmit = function(subject, topic, data) {};

    /** Set transmitter which will be transferring messages to another communication controller
     * @param {CommunicationControllerInterface} theTransmitter */
    this.setTransmitter = function(theTransmitter) {};

    /** Add messages receiver
     * @param {String} subject of the message to receive
     * @param {Function} receiver function*/
    this.addReceiver = function(subject, receiver) {};

    /** Remove messages receiver
     * @param {String} subject of the message to receive
     * @param {Function} receiver function*/
    this.removeReceiver = function(subject, receiver) {};
}

/** Communication classes factory. Used to create messages sender/recievers.
 * Instance can be found in {@link dcm.communicatino.factory} or {@link engine.communication.factory}
 * @param {DCMContext} context of the application where classes should be created
 * @constructor */
function CommunicationFactory(context) {
    /** Create instance of {@link MessageRecieverMixin}
     @param {String} subject component/class specific name for which messages should be accepted
     @param {String} [topic='.*'] regexp pattern or specifc message type that should be accepted. Accepts all messages from this component if ommited
     @return {MessageRecieverMixin}
     */
     this.createReciever = function(subject, topic) {
        return new MessageRecieverMixin(context, subject, topic);
    };

    /** Extend your class with methods of {@link MessageRecieverMixin}
     @param {Object} self pointer to class which should be extended
     @param {String} subject component/class specific name for which messages should be accepted
     @param {String} [topic='.*'] regexp pattern or specifc message type that should be accepted. Accepts all messages from this component if ommited
     */
    this.extendReciever = function(self, subject, topic) {
        MessageRecieverMixin.call(self, context, subject, topic);
    };

    /** Create instance of {@link MessageSenderMixin}
     @return {MessageSenderMixin}
     */
    this.createSender = function() {
        return new MessageSenderMixin(context);
    };

    /** Extend your class with methods of {@link MessageSenderMixin}
     * @param {Object} self pointer to class which should be extended
     * */
    this.extendSender = function(self) {
        MessageSenderMixin.call(self, context);
    };
  }

/** Debugging class which listen to messages and shows them using alerts
 * @param subject message name
 * @param [topic] message type
 * @class*/
function DebugMessageReceiver(subject, topic) {
    MessageRecieverMixin.call(this, subject, topic);

    this.onMessage = function(topic, data) {
        alert("subject: " + subject + "\ntopic:  " + topic + "\ndata:   " + data);
    }
}

/** Message data object. Allow to has custom serialize procedures.
 * @constructor */
function MessageData() {
    /** Override this method to return plain object that can be transmitted
     * @return {Object} */
    this.getJson = function() {
        return {};
    }
}

/**
 Allows to listen for messages sent by other components or client code. Can be used as mixin or standalone class.
 Avoid direct usage. Use context.communication.factory to create.
 @param {DCMContext} context dcm or engine context. Depends on location of user
 @param {String} subject component/class specific name for which messages should be accepted
 @param {String} [topic='.*'] regexp pattern or specifc message type that should be accepted. Accepts all messages from this component if ommited
 @constructor
 */
function MessageRecieverMixin(context, subject, topic) {
    var self = this;
    var acceptedTopic = new RegExp(topic ? topic : MESSAGING_DATA.ANY_TOPIC);

    function receiverFunction(topic, data) {
        if (acceptedTopic.test(topic)) {
            self.onMessage(topic, data);
        }
    }

    context.communication.controller.addReceiver(subject, receiverFunction);

    /** Execute to stop listening for the messages. */
    this.unregister = function() {
        context.communication.controller.removeReceiver(subject, receiverFunction)
    };

    /**
    * Override this method to recieve messages specified in the constructor subject/topic parameters
    * @param {String} topic type of recieved message
    * @param data data send by the component. */
    this.onMessage = function(topic, data) {};
}

/** Allows to send messages that can be recieved by {@link MessageRecieverMixin}
 * @constructor
 * */
function MessageSenderMixin(context) {

    function checkParameters(subject, topic) {
        if (!subject || !topic) {
            throw "Incorrect message parameters";
        }
    }

    /** Send required messages
     * @param {String} subject message name
     * @param {String} topic message type
     * @param [data=undefined] message data*/
    this.send = function(subject, topic, data) {
        checkParameters(subject, topic);
        context.communication.controller.send(subject, topic, data);
    };

    /** Transmit required messages to another context
     * @param {String} subject message name
     * @param {String} topic message type
     * @param {Object} data of the message. Should be JSON serializable*/
    this.transmit = function(subject, topic, data) {
        checkParameters(subject, topic);
        context.communication.controller.transmit(subject, topic, data);
    }
}

/** List of subject/topics which used in the client/engine communication.*/
var MESSAGING_DATA = {
    ANY_TOPIC : ".*",

    APPLICATION_LOG_SUBJECT : "application-log",

    REQUEST_MONITORING_SUBJECT : "monitoring-requests",
    REQUEST_MONITORING_DATA_READY : "request-data-availabale",

    PRIVACY_MODE_SUBJECT : "privacy-mode",
    PRIVACY_MODE_ENABLED : "entering-privacy-mode",
    PRIVACY_MODE_DISABLED : "exiting-privacy-mode",

    XHR_REMOTE_FUNCTION_SUBJECT : "xhr-executor-remote-function",
    REMOTE_DISK_STORAGE : "remote-disk-storage",
    REMOTE_SETTINGS_STORAGE : "remote-settings-storage",
    REMOTE_BROWSER_COMPONENT : "remote-browser-component",

    REMOTE_OBJECT_SUBJECT : "remote-objects",
    REMOTE_OBJECT_CREATE_TOPIC : "create-object",
    REMOTE_OBJECT_DESROY_TOPIC : "destroy-object",

    ENGINE_WORKER_SUBJECT : "engine-worker",
    WORKER_ERROR_TOPIC : "worker-code-error",
    WORKER_CODE_EXIST_TOPIC : "worker-class-ready",

    ENGINE_CREATE_TOPIC : "engine-create-command",
    ENGINE_CREATED_TOPIC : "engine-created-command",
    ENGINE_DESTROY_TOPIC : "engine-destroy-command",
    ENGINE_OPTIONS_TOPIC : "engine-options-command",
    ENGINE_CLEANUP_TOPIC : "engine-cleanup-command",

    SYSTEM_INFO_SUBJECT : "system_info_subject",
    LOCAL_IP_ADDRESS_DETECTED_TOPIC : "local_ip_address_detected_topic",

    OPT_IN_DIALOG_SUBJECT : "opt_in_dialog_subject",
    OPT_IN_DIALOG_CONFIG_AVAILABLE : "opt_in_dialog_config_available",
    OPT_IN_DIALOG_CONFIG_REMOVED : "opt_in_dialog_config_removed"
};


function ReceieversHandlerMixin() {
    var allReceivers = {};

    function getReceivers(subject) {
        if (!allReceivers[subject]) {
            allReceivers[subject] = [];
        }
        return allReceivers[subject];
    }


    this.addReceiver = function(subject, receiver) {
        var receivers = getReceivers(subject);
        receivers.push(receiver);
    };

    this.removeReceiver = function(subject, receiver) {
        var receivers = getReceivers(subject);
        var index = receivers.indexOf(receiver);
        if (index > -1) {
            receivers.splice(index, 1);
        }
    };

    this.notifyReceivers = function(subject, topic, data) {
        var receivers = getReceivers(subject);
        for (var i = 0; i < receivers.length; i++) {
            try {
                receivers[i](topic, data);
            } catch (e) {
                //ignore notifications errors for now
            }
        }
    }

}

/** Controller of remote calls for DCM registers Remote Objects. Create Remote Functions.
 * Use aleady created instance from {@link dcm.controller.remote} or {@link engine.controller.remote}
 * @param {DCMContext} context of the current controller
 * @constructor */
function RemoteCallsController(context) {
    var factory = new RemoteObjectsFactory(context);


    /** Register class as target for Remote Object calls
     * @param {Function} interfaceConstructor of Remote Object which has {@link RemoteObjectSpec}
     * @param {Function} targetConstructor constructor of Remote Object which extends interface and {@link RemoteObjectTarget}*/
    this.registerRemoteTarget = function(interfaceConstructor, targetConstructor) {
        factory.registerRemoteObject(interfaceConstructor, targetConstructor);
    };

    /** Create instance of {@link RemoteFunctionTarget} to process remote calls
     * @param {String} name of the remote function
     * @param {Function} callback to process remote calls
     * @return RemoteFunctionTarget*/
    this.createRemoteFunctionTarget = function(name, callback) {
        return new RemoteFunctionTarget(context, name, callback);
    };

    /** Create instance of {@link RemoteFunctionSource} to execute remote calls
     * @param {String} name of the remote function
     * @return RemoteFunctionSource */
    this.createRemoteFunctionSource = function(name) {
        return new RemoteFunctionSource(context, name)
    };

    /** Create instance of {@link RemoteFunctionQuietTarget} to process quiet remote calls
     * @param {String} name of the remote function
     * @param {Function} callback to process remote calls
     * @return RemoteFunctionQuietTarget*/
    this.createQuietRemoteFunctionTarget = function(name, callback) {
        return new RemoteFunctionQuietTarget(context, name, callback);
    };

    /** Create instance of {@link RemoteFunctionQuietSource} to execute one way remote calls
     * @param {String} name of the remote function
     * @return RemoteFunctionQuietSource */
    this.createQuietRemoteFunctionSource = function(name) {
        return new RemoteFunctionQuietSource(context, name)
    };

    /** Extend current Object with Remote Object Source functionality
     * @param {Object} self current class
     * @param {Array} args of the constructor*/
    this.extendRemoteObjectSource = function(self, args) {
        RemoteObjectSource.call(self, context, args);
    };

    /** Extend current Object with Remote Object Target functionality
     * @param {Object} self current class
     * @param {Object?} target of calls. leave blank to use self as target
     * */
    this.extendRemoteObjectTarget = function(self, target) {
        target = target ? target : self;
        RemoteObjectTarget.call(self, context, target);
    };
 }

/** Remote Function source which doesn't expect any response
 * @extend FunctionMessagesSender
 * @constructor*/
function RemoteFunctionQuietSource(context, name) {
    RemoteFunctionSource.call(this, context, name);

    var parentCall = this.callRemote;

    this.callRemote = function(data) {
        var args = this.convertArguments(arguments, 0);
        args.unshift(function() {});
        parentCall.apply(this, args);
    }
}

/** Remote Function quiet Target which doesn't provide any feedback
 * @extends RemoteFunctionTarget
 * @constructor */
function RemoteFunctionQuietTarget(context, name, method) {
    RemoteFunctionTarget.call(this, context, name, quietCallback);
    var self = this;

    function quietCallback(callback) {
        var args = self.convertArguments(arguments, 1);
        method.apply(method, args);
        callback(true);
    }
}

/** Performs Remote Function calls by name. Do not create manually use {@link CommunicationFactory}
 * @param {DCMContext} context current context
 * @param {String} name of the remote function
 * @extends MessageRecieverMixin
 * @extends RemoteUtils
 * @constructor*/
function RemoteFunctionSource(context, name) {
    RemoteUtils.call(this);
    context.communication.factory.extendReciever(this, name+"-response");

    var callerId = this.generateId();
    var callCount = 0;
    var callbacks = {};
    var sender = context.communication.factory.createSender();

    function getCallId() {
        callCount++;
        return callerId+"."+callCount;
    }

    /** Perform call to Remote Function
     * @param {Function} callback to receive results
     * @param {Object} data to send in remote function*/
    this.callRemote = function(callback, data) {
        var args = this.convertArguments(arguments, 1);
        var callId = getCallId();
        callbacks[callId] = callback;
        sender.transmit(name+"-call", callId, args);
    };

    this.onMessage = function responseArrived(topic, data) {
        data = data.slice();
        var callback = callbacks[topic];
        callback.apply(callback, data);
        delete callbacks[topic];
    };
}

/** Receives remote calls by name and forwards execution to the method. Do not create manually use {@link CommunicationFactory}
 * @param {DCMContext} context current context
 * @param {String} name of Remote function
 * @param {Function} method to process remote calls
 * @extends MessageRecieverMixin
 * @extends RemoteUtils
 * @constructor */
function RemoteFunctionTarget(context, name, method) {
    RemoteUtils.call(this);

    var self = this;
    var sender = context.communication.factory.createSender();
    context.communication.factory.extendReciever(this, name+"-call");

    function generateResponseCallback(topic) {
        return function() {
            var args = self.convertArguments(arguments, 0);
            sender.transmit(name+"-response", topic, args);
        }
    }

    this.onMessage = function(topic, data) {
        data = data.slice();
        data.unshift(generateResponseCallback(topic));
        method.apply(method, data);
    }
}

/** Handles construction of Remote objects. Contain list of registered Remote Objects
 * @constructor */
function RemoteObjectsFactory(context) {
    context.communication.factory.extendReciever(this, MESSAGING_DATA.REMOTE_OBJECT_SUBJECT);

    var logger = context.loggerFactory.getLogger("communication.RemoteObjectsFactory");

    var remoteObjects = {};
    var remoteConstructors = {};

    function construct(constructor, args) {
        function F() {
            return constructor.apply(this, args);
        }
        F.prototype = constructor.prototype;
        return new F();
    }

    function constructObject(data) {
        logger.debug("Constructing remote target: "+data.interfaceName);
        var objectConstructor = remoteConstructors[data.interfaceName];
        if (objectConstructor) {
            try {
                var args = data.constructorArguments;
                args.unshift(context);
                var instance = construct(objectConstructor, args);
                var instanceId = data.instancesId;
                instance.initRemoteTarget(instanceId);
                remoteObjects[instanceId] = instance;
                logger.debug("Remote target constructed.");
            } catch (e) {
                logger.error("Cannot construct remote target: "+e);
            }
        } else {
            logger.debug("Remote target is not registered.")
        }
    }

    function destroyObject(instanceId) {
        var object = remoteObjects[instanceId];
        if (object) {
            logger.debug("Destroying remote target: "+instanceId);
            delete remoteObjects[instanceId];
            object.destroy();
        } else {
            logger.debug("Cannot destroy remote target. Instance not found: "+instanceId);
        }
    }

    this.onMessage = function(topic, data) {
        if (topic == MESSAGING_DATA.REMOTE_OBJECT_CREATE_TOPIC) {
            constructObject(data);
        } else if (topic == MESSAGING_DATA.REMOTE_OBJECT_DESROY_TOPIC) {
            destroyObject(data);
        }
    };

    this.registerRemoteObject = function(interfaceConstructor, targetConstructor) {
        var interfaceInstance = new interfaceConstructor();
        logger.info("Registering remote target: "+interfaceInstance.getRemoteName());
        remoteConstructors[interfaceInstance.getRemoteName()] = targetConstructor;
    }
}

/** Extend this object to receive functionality of Remote Object source.
 * Your object should extend {@link RemoteObjectSpec}.
 * As result if you will call methods specified in specification target will be called automatically.
 * Do not create manually use {@link RemoteCallsController}
 * @param {DCMContext} context of your applicaiton
 * @param {Arguments} constructorArguments passed during creation of your object.
 * @constructor*/
function RemoteObjectSource(context, constructorArguments) {
    var self = this;
    var remoteFunctions = [];
    var sender = context.communication.factory.createSender();

    createResponseMethods(this.getResponseMethods(), "createRemoteFunctionSource");
    createResponseMethods(this.getQuietMethods(), "createQuietRemoteFunctionSource");
    createRemoteObject();

    function createRemoteObject() {
        var args = self.convertArguments(constructorArguments, 0);
        sender.transmit(MESSAGING_DATA.REMOTE_OBJECT_SUBJECT, MESSAGING_DATA.REMOTE_OBJECT_CREATE_TOPIC, {
            interfaceName: self.getRemoteName(),
            instancesId: self.getInstanceId(),
            constructorArguments: args})
    }

    function createResponseMethods(methodNames, constructionName) {
        for (var i = 0; i < methodNames.length; i++) {
            var methodName = methodNames[i];
            var methodId = self.constructMethodId(methodName);
            var func = context.communication.remote[constructionName](methodId);
            remoteFunctions.push(func);
            createMethod(methodName, func);
        }
    }

    function createMethod(name, remoteFunction) {
        self[name] = function() {
            var args = self.convertArguments(arguments, 0);
            remoteFunction.callRemote.apply(remoteFunction, args);
        }
    }

    this.destroy = function() {
        sender.transmit(MESSAGING_DATA.REMOTE_OBJECT_SUBJECT, MESSAGING_DATA.REMOTE_OBJECT_DESROY_TOPIC, self.getInstanceId());
        for (var i = 0; i < remoteFunctions.length; i++) {
            remoteFunctions[i].unregister();
        }
    }
}

/** Specification of Object which can be instantiated and interacted remotely
 *  Interfaces should extend this Class to allow Remote Functions call between instances.
 * @param {Object} specJson of this object
 * @param {String} specJson.name unique name of Object class.
 * @param {Array} specJson.quietMethods method names which doesn't provide any response
 * @param {Array} specJson.responseMethods method names which provide response
 * @extend RemoteUtils
 * @constructor
 * */
function RemoteObjectSpec(specJson) {
    RemoteUtils.call(this);
    var instanceId = specJson.name+"-"+this.generateId();

    /** Get unique name of Remote Object class
     * @return {String}*/
    this.getRemoteName = function() {
        return specJson.name;
    };

    /** Get Unique id for new instance
     * @return {String}*/
    this.getInstanceId = function() {
        return instanceId;
    };

    /** Set instance id on target side*/
    this.setInstanceId = function(id) {
        instanceId = id;
    };

    /** Get list of messages wich doesn't produce response
     * @return {String[]}*/
    this.getQuietMethods = function() {
        return specJson.quietMethods;
    };

    /** Get list of methods which provide response
     * @return {String[]}*/
    this.getResponseMethods = function() {
        return specJson.responseMethods;
    };

    /** Construct unique method name
     * @param {String} methodName to construct
     * @return {String}*/
    this.constructMethodId = function(methodName) {
        return instanceId+"-"+methodName;
    }
  }

/** Extend this object to receive functionality of Remote Object target.
 * Your object should extend {@link RemoteObjectSpec}.
 * Constructor of your object should be registered in {@link RemoteCallsController}
 * As result methods specified in specification will be called when remote source calling methods.
 * Do not create manually use {@link RemoteCallsController}
 * @param {DCMContext} context of your applicaiton
 * @param {Object} target where methods should be called. Use this for own methods.
 * @constructor*/
function RemoteObjectTarget(context, target) {
    var self = this;
    var remoteFunctions = [];

    function createMethodConnectors(methodsNames, constructionName) {
        for (var i = 0; i < methodsNames.length; i++) {
            var methodName = methodsNames[i];
            var methodId = self.constructMethodId(methodName);
            var func = context.communication.remote[constructionName](methodId, createConnector(methodName));
            remoteFunctions.push(func);
        }
    }

    function createConnector(name) {
        return function() {
            var args = self.convertArguments(arguments, 0);
            target[name].apply(self, args);
        }
    }

    /** Init target with instance id identical to the source. This allow one to one communication.
     * Called automatically for registered Objects.
     * @param {String} instanceId of current target*/
    this.initRemoteTarget = function(instanceId) {
        this.setInstanceId(instanceId);
        createMethodConnectors(this.getResponseMethods(), "createRemoteFunctionTarget");
        createMethodConnectors(this.getQuietMethods(), "createQuietRemoteFunctionTarget");
    };

    this.destroy = function() {
        for (var i = 0; i < remoteFunctions.length; i++) {
            remoteFunctions[i].unregister();
        }
    }
}

/** Remote calls utils. Contain use-full methods to work with Remote classes
 * @constructor */
function RemoteUtils() {
    /** Retrieve array of arguments from Arguments object starting at specific position
     * @param {Arguments|Array} args from which to retrieve
     * @param {Number} start of the first argument
     * @return {Array} of arguments */
    this.convertArguments = function(args, start) {
        return Array.prototype.splice.call(args, start);
    };

    /** Generate unique id based on current timestamp and random number*/
    this.generateId = function() {
        return Date.now()+"-"+Math.floor(Math.random() * 10000)
    }
}


/** Perform communication with WebWorker.
 * @param {Worker} worker to decorate.
 * @extends CommunicationControllerInterface
 * @constructor*/
function WorkerCommunicator(worker) {
    CommunicationControllerInterface.call(this);
    ReceieversHandlerMixin.call(this);
    var self = this;
    var transmitter;

    function messageListener(event) {
        var messageData = event;
        if (!event.subject) {
            messageData = event.data;
        }

        self.notifyReceivers(messageData.subject, messageData.topic, messageData.data);

        if (transmitter) {
            transmitter.receive(messageData.subject, messageData.topic, messageData.data);
        }
    }

    function errorListener(event) {
        var message = '"'+event.message + ". At line: " + event.lineno+'"';
        self.notifyReceivers(MESSAGING_DATA.ENGINE_WORKER_SUBJECT, MESSAGING_DATA.WORKER_ERROR_TOPIC, message);

        if (transmitter) {
            transmitter.receive(MESSAGING_DATA.ENGINE_WORKER_SUBJECT,
                MESSAGING_DATA.WORKER_ERROR_TOPIC, message);
        }
    }

    function setListeners(onMessage, onError) {
        if(worker.on) {
            worker.on("message", onMessage);
        } else {
            worker.onmessage = onMessage;
        }

        if(worker.on) {
            worker.on("error", onError)
        } else {
            worker.onerror = onError;
        }
    }

    this.start = function() {
        setListeners(messageListener, errorListener);
    };

    this.stop = function() {
        setListeners(function(){}, function(){});
    };

    this.receive = function(subject, topic, data) {
        this.notifyReceivers(subject, topic, data);
        worker.postMessage({
            subject: subject,
            topic: topic,
            data: data
        });
    };

    this.setTransmitter = function(theTransmitter) {
        transmitter = theTransmitter;
    }
}

/** Interface of the Logger Factory.
 * @constructor */
function LoggerFactoryInterface() {

    /** Get instance of logger for your class
     * @param {String} className of the class that will be performing logging operations
     * @return {LoggerInterface} */
     this.getLogger = function(className) {
        return new LoggerInterface()
    };

    /**
     * Add appender to logger factory
     * @param appender */
    this.addAppender = function(appender) {
        throw "Method is not implemented";
    };

    /** Clear the logged data.
     * In case the log is a file, the file should be removed */
    this.clear = function() { }
}

/** Data Logger interface. Actual implementation accordinly to settings will be writing log entries to some log file
 * @constructor*/
function LoggerInterface() {
    /** Logs debug message
     * @param {string} message*/
    this.debug = function(message) {};
    /** Logs info message
     * @param {string} message*/
    this.info = function(message) {};
    /** Logs warn message
     * @param {string} message*/
    this.warn = function(message) {};
    /** Logs error message
     * @param {string} message*/
    this.error = function(message) {};
}


/** Holder of the request data that represents click stream
 * @param {Object} [jsonData] The JSON object that is used for request data initializing
 * @constructor */
function RequestData(jsonData) {
    var data = jsonData instanceof Object ? jsonData : {};

    if (!data.timestamp) {
        data.timestamp = Date.now();
    }

    this.setUrl = function(url) {
        data.url = url;
    };

    this.getUrl = function() {
        return data.url;
    };

    this.setTabId = function(tabId) {
        data.tabId = tabId;
    };

    this.getTabId = function() {
        return data.tabId;
    };

    this.setMethod = function(theMethod) {
        data.method = theMethod;
    };

    this.getMethod = function() {
        return data.method;
    };

    this.getTimestamp = function() {
        return data.timestamp;
    };

    this.setTimestamp = function(timestamp) {
        data.timestamp = timestamp;
    };

    this.setRequestType = function(requestType) {
        data.requestType = requestType;
    };

    this.getRequestType = function() {
        return data.requestType;
    };

    this.setPostData = function(postData) {
        data.postData = postData;
    };

    this.getPostData = function() {
        return data.postData;
    };

    this.setStatusCode = function(statusCode) {
        data.status = statusCode;
    };

    this.getStatusCode = function() {
        return data.status;
    };

    this.setFrameId = function(frameId) {
        data.frameId = frameId;
    };

    this.getFrameId = function() {
        return data.frameId;
    };

    this.setOpenerTabId = function(refId) {
        data.openerTabId = refId;
    };

    this.getOpenerTabId = function() {
        return data.openerTabId;
    };

    this.setRequestHeaders = function(headers) {
        data.requestHeaders = headers;
    };

    this.getRequestHeaders = function() {
        return data.requestHeaders;
    };

    this.setResponseHeaders = function(headers) {
        data.responseHeaders = headers;
    };

    this.getResponseHeaders = function() {
        return data.responseHeaders;
    };

    /** Get all data in JSON object
     * @return {Object} json object */
    this.getData = function() {
        return data;
    };
}

/** Remote Settings Interface. Defines remote methods.
 * @extends SettingsInterface
 * @extends RemoteObjectSpec
 * @constructor */
function RemoteSettingsInterface() {
    SettingsInterface.call(this);
    RemoteObjectSpec.call(this, {
        name: MESSAGING_DATA.REMOTE_SETTINGS_STORAGE,
        quietMethods: ["set", "remove", "removeAll"],
        responseMethods: []
    })
}


/** Remote Storage Interface. Defines remote methods.
 * @extends StorageInterface
 * @extends RemoteObjectSpec
 * @constructor */
function RemoteStorageInterface() {
    StorageInterface.call(this);
    RemoteObjectSpec.call(this, {
        name: MESSAGING_DATA.REMOTE_DISK_STORAGE,
        quietMethods: [],
        responseMethods: ["read", "write", "exist", "remove"]
    })
}


/** List of commonly used settings across engine.
 * Must contain only settings that should be available in both client and engine.
 * DO NOT add new settings that is accessible only from one context */
var SETTINGS = {
    CLIENT_VERSION: "version.client",
    ENGINE_VERSION: "version.engine",
    USER_ID: "user-id",
    GROUP_ID: "group-id",

    DCM_CONFIG_NAME: "DCM Config",
    DCM_CONFIG_VERSION: "DCM Config.version",

    CLIENT_VERSION_HEADER: "X-Client-Version",
    ENGINE_VERSION_HEADER: "X-Engine-Version",
    USER_ID_HEADER: "X-User-Id",
    GROUP_ID_HEADER: "X-Group-Id",
    DCM_CONFIG_VERSION_HEADER: "X-Dcm-Config",

    DATA_DISABLED_KEY: "data-capturing-disabled",
    EPOCH_OFFSET: "epoch-offset",

    LOCAL_ADDRESS: "local-address",
    CONFIRMATION_PAGE_SHOWN: "confirmation-page-shown",

    DIAGNOSTIC_URL: "https://collector.dataferb.com/diagnostic"
};

/** Operates with permanent toolbar settings
 * @constructor */
function SettingsInterface() {
    /** Set setting value
     * @param {string} key name of the setting
     * @param {(string|number|boolean)} value of the setting*/
    this.set = function(key, value) {};

    /** Get setting value
     * @param {string} key name of the setting
     * @return {(string|number|boolean|undefined)}*/
    this.get = function(key) {};

    /** Check if setting exists
     * @param {string} key name of the setting
     * @return {boolean}
     */
    this.has = function(key) {};

    /** Remove specified setting
     * @param {string} key name of the setting
     * */
    this.remove = function(key) {};

    /** <b>Remove all settings</b> for toolbar. <b>Important:</b> Do not use this mmethod in regular code*/
    this.removeAll = function() {};

    /** Get all settings
     * @return {Object} ket/value mapped object */
    this.getAll = function() {};
}


/**Manages access to FileStorage and Settings
 * @param {String} settingsRoot being used to distinguish between DCM settings and others toolbar
 * @constructor */
function StorageFactoryInterface(settingsRoot) {

    /** Creates File Storage that provides access to the File System
     * @param {String} name of the file
     * @return {StorageInterface} */
    this.getStorage = function(name) {};

    /** Creates Settings Storage that provides access to the toolbar's related preferences
     * @return {SettingsInterface} */
    this.getSettings = function() {}
}

/** Permanent Large volume Storage interface
 * @constructor */
function StorageInterface() {
    /** Read saved data
     * @param {Function} callback to be called when data available */
    this.read = function(callback) {};

    /** Write saved data
     * @param {Function} callback to be called when data saved with status
     * @param {String} value to save in storage*/
    this.write = function(callback, value) {};

    /** Writes data to the end of existing storage
     * @param {Function} callback to be called with writing status when data saved
     * @param {String} value to save */
    this.append = function(callback, value) {};

    /** Check if data available
     * @param {Function} callback to be called when data saved. */
    this.exist = function(callback) {};

    /** Remove stored data
     * @param {Function} callback to be called when data is removed*/
    this.remove = function(callback) {};
}


/** Tracks interval download interval for  specific config
 * @param {SettingsInterface} settings
 * @param {String} settingName
 * @param {Number} downloadInterval Interval in milliseconds
 * @constructor */
function IntervalChecker(settings, settingName, downloadInterval) {
    var callback;
    var timeoutId;

    function getLastCheckTime() {
        var dateString = settings.get(settingName);
        try {
            return parseInt(dateString, 10);
        } catch (e) {
            return 0
        }
    }

    function setLastCheckTime() {
        var dateString = Date.now().toString();
        settings.set(settingName, dateString);
    }

    function getNextInterval() {
        var lastTime = getLastCheckTime();
        if (lastTime > 0) {
            var timeSinceLastCheck = Math.abs(Date.now() - lastTime);
            if (timeSinceLastCheck < downloadInterval) {
                return downloadInterval - timeSinceLastCheck;
            }
        }

        return 0;
    }

    function scheduleNextCheck(interval) {
        timeoutId = setTimeout(notify, interval);
    }

    function clearNextCheck() {
        if (timeoutId) {
            clearTimeout(timeoutId);
            timeoutId = null;
        }
    }

    function notify() {
        timeoutId = null;
        setLastCheckTime();
        scheduleNextCheck(downloadInterval);
        callback();
    }

    /** Start interval checker
     * @param {Function} theCallback function that will be called when next interval has come */
    this.start = function(theCallback) {
        callback = theCallback;

        scheduleNextCheck(getNextInterval());
    };

    /** Stop interval checks */
    this.stop = function() {
        callback = null;
        clearNextCheck();
    };

    /**
     * Updates last download time value
     */
    this.update = function() {
        if (callback) {
            throw "Update method should be called before start";
        }

        setLastCheckTime();
    };

    /** Returns interval status. True - no need to call callback.
     * False - should call callback
     * @return {boolean} */
    this.isUpToDate = function() {
        return getNextInterval() > 0;
    }
}


/**  Wrapper for URL specific functionality
 * @param {String} theUrl
 * @constructor */
function Url(theUrl) {
    var utils = new UrlUtils();
    var urlParts = utils.parseUrl(theUrl);
    var urlParams = utils.splitParams(urlParts.query);

    /** Returns array of query parameters
     * @return {Object} in key:value format */
    this.getParams = function() {
        return urlParams;
    };

    /** Set the query string for URL
     * @param {Object} params */
    this.setParams = function(params) {
        urlParams = params;
    };

    /** Return string representation of URL
     * @return {String} */
    this.toString = function() {
        var params = utils.joinParams(urlParams);

        if (urlParts.query.length > 0) {
            return theUrl.replace(urlParts.query, params);
        } else if (params.length > 0) {
            return theUrl + "?" + params;
        }

        return theUrl;
    };

    /** Return domain name
     * @return {String} */
    this.getDomain = function() {
        return urlParts.host.replace(/^www\./, "");
    };

    /** Get the original (unmodified) URL
     * @return {String} */
    this.getOriginalUrl = function() {
        return theUrl;
    }
}


/** Contains URL related functionality
 * @constructor */
function UrlUtils() {

    /** Parse URL into url parts:
     * "source", "protocol", "authority", "userInfo", "user", "password", "host",
     * "port", "relative", "path", "directory", "file", "query", "anchor"
     * @param {String} sourceUrl
     * @return {Object} */
    this.parseUrl = function(sourceUrl) {
        var options = {
            strictMode : true,
            key : ["source", "protocol", "authority", "userInfo", "user", "password", "host", "port", "relative", "path", "directory", "file", "query", "anchor"],
            q : {
                name : "queryKey",
                parser : /(?:^|&)([^&=]*)=?([^&]*)/g
            },
            parser : {
                strict : /^(?:([^:\/?#]+):)?(?:\/\/((?:(([^:@]*)(?::([^:@]*))?)?@)?([^:\/?#]*)(?::(\d*))?))?((((?:[^?#\/]*\/)*)([^?#]*))(?:\?([^#]*))?(?:#(.*))?)/,
                loose : /^(?:(?![^:@]+:[^:@\/]*@)([^:\/?#.]+):)?(?:\/\/)?((?:(([^:@]*)(?::([^:@]*))?)?@)?([^:\/?#]*)(?::(\d*))?)(((\/(?:[^?#](?![^?#\/]*\.[^?#\/.]+(?:[?#]|$)))*\/?)?([^?#\/]*))(?:\?([^#]*))?(?:#(.*))?)/
            }
        };

        var m = options.parser[options.strictMode ? "strict" : "loose"].exec(sourceUrl),
            uri = {},
            i = 14;

        while (i--) uri[options.key[i]] = m[i] || "";

        uri[options.q.name] = {};
        uri[options.key[12]].replace(options.q.parser, function($0, $1, $2) {
            if ($1) uri[options.q.name][$1] = $2;
        });

        return uri;
    };

    /** Split query string into key:value array of parameters
     * @param {String} queryString
     * @return {Object} */
    this.splitParams = function(queryString) {
        var result = {};
        if (queryString) {
            var params = queryString.split(/([;\?&#])/);

            for (var i = 0, length = params.length; i < length; i++) {
                var pair = params[i];
                var pos = pair.indexOf('=');

                if (pos > 0) {
                    var key = pair.substring(0, pos);
                    result[key] = pair.substring(pos + 1);
                }
            }
        }

        return result;
    };

    /** Merge object parameters into string
     * @param {Object} theParams
     * @return {String} */
    this.joinParams = function(theParams) {
        var result = "";

        for (var param in theParams) {
            result += param + "=" + theParams[param] + "&";
        }

        return result.substring(0, result.lastIndexOf("&"));
    }
}


function isUpdate(oldVersion, newVersion) {
    function normalizeVersion(versionString) {
        var maxWidth = 4;
        var updateVersionString = "";

        versionString.split('.').forEach(function (num) {
            var nil = "";
            var delta = maxWidth - num.length;

            if (delta > 0) {
                nil = new Array(delta + 1).join("0");
            }
            updateVersionString += nil + num + '.';
        });

        if (updateVersionString.length > 0) {
            return updateVersionString.substring(0, updateVersionString.length - 1);
        } else {
            return versionString;
        }
    }

    return normalizeVersion(oldVersion) < normalizeVersion(newVersion);
}

/** Interface of the class which able to send XMLHttpRequest and give the response
 * @constructor */
function XhrExecutorInterface() {
    /** Perform request
     * @param {Object|String} data to send with request
     * @param {Boolean} async An optional boolean parameter
     * indicating whether or not to perform the operation asynchronously. */
     this.send = function(data, async) {};

    /** Set header that will send with request
     * @param {String} name of the header
     * @param {String} value of the header */
    this.setRequestHeader = function(name, value) {};

    /** Set headers to disable 304 cache response*/
    this.disableCaching = function() {}
}


/** Interface of the factory to create XHR requests.
 * @constructor*/
function XhrFactoryInterface() {

    /** Create Get request
     * @param {String} url of the request
     * @param {Function} callback to notify about response
     * @return {XhrExecutorInterface} executor */
    this.createGet = function(url, callback) {
        return new XhrExecutorInterface();
    };

    /** Create Post request
     * @param {String} url of the request
     * @param {Function} callback to notify about response
     * @return {XhrExecutorInterface} executor */
    this.createPost = function(url, callback) {
        return new XhrExecutorInterface();
    };

    /** Create Xhr Response
     * @param {Number} code of the response
     * @param {String|Null} text of the response
     * @return {XhrResponse}*/
    this.createResponse = function(code, text) {
        return new XhrResponse({code: code, text: text});
    }
 }

/** Xhr Response object. Use {@link XhrFactoryInterface} to create instances.
 * @param {Object} responseJson with represents all data
 * @param {Number} responseJson.code of the response
 * @param {String|Null} [responseJson.text] of the response
 * @constructor */
function XhrResponse(responseJson) {
    this.isSuccessful = function() {
        return this.isOkResponse() || this.isNotModifiedResponse();
    };

    this.getText = function() {
        return this.isOkResponse() ? responseJson.text : null;
    };

    this.isOkResponse = function() {
        return responseJson.code == 200;
    };

    this.isNotModifiedResponse = function() {
        return responseJson.code == 304;
    };

    this.getJson = function() {
        return responseJson;
    };

    this.getCode = function() {
        return responseJson.code
    }
}

/** Component responsible for managing add-on status (i.e enabled/disabled)
 * @module client/addon
 * @require client/storage
 * @require client/diagnostic
 * @require client/communication
 * @extends StorageInterface
 * @constructor */
 function AddonComponent(context) {
    ComponentInterface.call(this);

    var isDisabled = false;

    function toggleState(enabled) {
        var topic = enabled ? MESSAGING_DATA.DCM_ENABLED : MESSAGING_DATA.DCM_DISABLED;

        var sender = context.communication.factory.createSender();
        sender.send(MESSAGING_DATA.CLIENT_SYNC_DIAGNOSTIC_SUBJECT, topic);

        var settings = context.storageFactory.getSettings();
        settings.set("disabled", !enabled);
    }

    this.create = function(config) {
        var settings = context.storageFactory.getSettings();
        if (settings.get("disabled")) {
            toggleState(true);
        }
    };

    this.destroy = function() {
        if (isDisabled) {
            toggleState(false);
        }
    };

    this.options = function(options) {
        if (options && options.addonDisabled !== undefined) {
            isDisabled = options.addonDisabled;
        }
    };
}



MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT = "client-diagnostic-subject";
MESSAGING_DATA.CLIENT_SYNC_DIAGNOSTIC_SUBJECT = "client-sync-diagnostic-subject";

MESSAGING_DATA.ENGINE_INVALIDATION_SUBJECT = "engine_invalidation_subject";
MESSAGING_DATA.ENGINE_INVALIDATION_TOPIC = "engine_invalidation_topic";

MESSAGING_DATA.ENGINE_DOWNLOAD_SUCCEEDED = "dcm-engine-download-succeeded";
MESSAGING_DATA.ENGINE_DOWNLOAD_FAILED = "dcm-engine-download-failed";
MESSAGING_DATA.ENGINE_FAILURE = "dcm-engine-failure";
MESSAGING_DATA.CLIENT_INSTALLED = "dcm-client-installed";
MESSAGING_DATA.CLIENT_UPDATED = "dcm-client-updated";

MESSAGING_DATA.CLIENT_UNINSTALLED = "dcm-client-uninstalled";
MESSAGING_DATA.USER_OPTED_IN = "dcm-user-opted-in";
MESSAGING_DATA.USER_OPTED_OUT = "dcm-user-opted-out";

MESSAGING_DATA.USER_ID_RESTORED = "dcm-user-id-restored";

MESSAGING_DATA.DCM_DISABLED = "dcm-disabled";
MESSAGING_DATA.DCM_ENABLED = "dcm-enabled";

MESSAGING_DATA.TAB_STATE_CHANGE_SUBJECT = "tab-state-change-subject";
MESSAGING_DATA.TAB_VISIBLE = "tab_visible";
MESSAGING_DATA.TAB_HIDDEN = "tab_hidden";
MESSAGING_DATA.TAB_PRERENDER = "tab_prerender";
MESSAGING_DATA.TAB_RELOADED = "tab_reloaded";

MESSAGING_DATA.HOST_VERSION_UPDATED = "dcm-host-version-updated";

MESSAGING_DATA.PORT_LISTENER_SUBJECT = "port-listener-subject";
MESSAGING_DATA.TAB_VISIBILITY_CHANGED = "on-visibility-changed";
MESSAGING_DATA.CONTEXT_MENU_SHOWN = "on-context-menu-shown";
MESSAGING_DATA.PORT_DISCONNECTED = "port-disconnected";

MESSAGING_DATA.ENGINE_STOPPED = "dcm-engine-stopped";
MESSAGING_DATA.TAB_CREATED = "dcm-tab-created";
MESSAGING_DATA.TAB_CLOSED = "dcm-tab-closed";


/** Controller of DCM client
 * @constructor */
function DcmApi(context) {
    var started = false;
    var doCleanup = false;

    var configuration = {};

    /** Start DCM client
     * @throws {Error} if called several times */
    this.start = function() {
        if (started) {
            throw "DCM cannot be started twice";
        }
        context.create(configuration);
        started = true;
    };

    /** Stop DCM client
     * @throws {Error} if called before start */
    this.stop = function() {
        if (!started) {
            throw "DCM cannot be stopped. It wasn't started";
        }

        if (doCleanup) {
            context.cleanup();
        }

        context.destroy();
        started = false;
    };

    /** Uninstall DCM client
     * @throws {Error} if called before start */
    this.uninstall = function() {
        if (!started) {
            throw "DCM cannot be uninstalled. It wasn't started";
        }

        doCleanup = true;
    };

    /** Cancel pending Uninstall procedures
     * @throws {Error} if called before start
     * @throws {Error} if called before uninstall */
    this.cancelUninstall = function() {
        if (!started) {
            throw "DCM cannot be uninstalled. It wasn't started";
        }

        if (!doCleanup) {
            throw "Cannot cancel uninstall since it hasn't been called.";
        }

        doCleanup = false;
    };

    /** Configure DCM. Should be called before start.
     * @param {Object} dcmConfiguration configuration object
     * @param {String} dcmConfiguration.userId dcm installation user id
     * @param {String} dcmConfiguration.groupId dcm installation group id
     * @throws {Error} if called after start */
    this.configure = function(dcmConfiguration) {
        if (started) {
            throw "DCM cannot be configured. It was already started";
        }

        configuration = dcmConfiguration;
    };

    /** Set Opt-in status for data capturing. Should be called after start.
     * Setting remembered till next call
     * @param {Boolean} isEnabled - data capturing
     * @throws {Error} if called before start */
    this.permitDataCapturing = function(isEnabled) {
        if (!started) {
            throw "DCM cannot accept Opt-In setting before it was started";
        }
        context.options({dataCapturingDisabled : !isEnabled});
    };

    /** Show the user Opt-In dialog with specified content and
     * sets Opt-In status for data capturing. Should be called after start.
     * @param {Object} dialog The object that contains all the dialog data
     * @param {String} dialog.url - html string to be shown to user
     * @param {String} dialog.title - title of the dialog
     * @param {String} dialog.accept Text for "Accept" button
     * @param {String} dialog.reject Text for "Reject" button */
    this.requestUserOptIn = function(dialog) {
        if (!started) {
            throw "DCM cannot show user Opt-In dialog before start";
        }

        context.options({optInDialog : dialog});
    };

    /** Notifies DCM about changes in add-on status (i.e. enabled/disabled)
     * @param {Boolean} isDisabled - add-on state, true - add-on has been just disabled,
     * false - add-on has been just enabled
     * @throws {Error} if called before start */
    this.onDisable = function(isDisabled) {
        if (!started) {
            throw "DCM cannot be notified about enable/disable before start";
        }
        context.options({addonDisabled : isDisabled});
    };

    /** Notifies DCM about version of the host toolbar
     * DMC doesn't react if the version doesn't change
     * @param {String} version
     * @throws {Error} if called before start */
    this.setHostVersion = function(version) {
        if (!started) {
            throw "DCM cannot be notified about host version before start";
        }
        context.options({hostVersion : version});
    }
}


function DiagnosticComponent(context) {
    ComponentInterface.call(this);

    var asyncSender;
    var syncSender;

    this.create = function(config) {
        asyncSender = new DiagnosticEventSender(context, MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT, true);
        syncSender = new DiagnosticEventSender(context, MESSAGING_DATA.CLIENT_SYNC_DIAGNOSTIC_SUBJECT, false);

        context.diagnostic = {};
        context.diagnostic.onReady = function() {
            asyncSender.onReady();
            syncSender.onReady();
        }
    };

    this.destroy = function() {
        if (asyncSender) {
            asyncSender.unregister();
        }

        if (syncSender) {
            syncSender.unregister();
        }
    }
}


/** Listen to the runner messages and sends them as diagnostic events.
 * @param {DCMContext} context
 * @param {String} subject of the listening message
 * @param {Boolean} isAsync The flag that tells whether all events should be asynchronous
 * @extends MessageRecieverMixin
 * @constructor */
function DiagnosticEventSender(context, subject, isAsync) {
    context.communication.factory.extendReciever(this, subject);
    var settings = context.storageFactory.getSettings();

    var messages = [];
    var isReady = false;

    function getOffset() {
        var offset = parseInt(settings.get(SETTINGS.EPOCH_OFFSET));
        return isNaN(offset) ? 0 : offset;
    }

    function createDataString(topic, data) {
        return JSON.stringify({
            event : topic,
            timestamp : Date.now() - getOffset(),
            message : data ? data : ""
        });
    }

    function sendMessage(message) {
        var xhr = context.requestFactory.createPost(SETTINGS.DIAGNOSTIC_URL, function() {});
        xhr.send(message, isAsync);
    }

    this.onReady = function() {
        isReady = true;

        messages.forEach(function(msg) {
            sendMessage(msg);
        });

        messages = [];
    };

    this.onMessage = function(topic, data) {
        if (!isReady) {
            messages.push(createDataString(topic, data));
        } else {
            sendMessage(createDataString(topic, data));
        }
    };
}

function EngineCachingComponent(context) {
    ComponentInterface.call(this);

    var CHECK_INTERVAL = 24 * 60 * 60 * 1000; // 24 hours

    var controller;
    var invalidationSource;

    this.create = function(config) {
        var downloader = new EngineDownloader(context);
        controller = new EngineCachingController(context, downloader);

        var settings = context.storageFactory.getSettings();
        var intervalChecker = new IntervalChecker(settings, SETTINGS.ENGINE_LAST_DOWNLOAD_TIME, CHECK_INTERVAL);

        invalidationSource = new EngineInvalidationSource(context, intervalChecker);
        invalidationSource.start();
    };

    this.destroy = function() {
        invalidationSource.stop();
    };
}

/** Controls the caching for engine file
 * @param {DCMContext} context
 * @param {EngineDownloader} downloader
 * @constructor
 */
function EngineCachingController(context, downloader) {
    context.communication.factory.
        extendReciever(this, MESSAGING_DATA.ENGINE_INVALIDATION_SUBJECT);

    var logger = context.loggerFactory.getLogger("client.downloader.EngineLifecycleController");
    var storage = context.storageFactory.getStorage(SETTINGS.ENGINE_NAME);
    var sender = context.communication.factory.createSender();

    var isBusy = false;

    storage.exist(function(hasContent) {
        if (hasContent) {
            sendCodeExistMessage();
        }
    });

    function sendCodeExistMessage() {
        setTimeout(function(){
            sender.send(MESSAGING_DATA.ENGINE_WORKER_SUBJECT,
                MESSAGING_DATA.WORKER_CODE_EXIST_TOPIC);
        }, 100);
    }

    function sendEngineDownloadSucceeded() {
        sender.send(MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT,
            MESSAGING_DATA.ENGINE_DOWNLOAD_SUCCEEDED);
    }

    function notifyCodeReady() {
        sendCodeExistMessage();
        sendEngineDownloadSucceeded();
    }

    function onSuccessfulDownload(response) {
        logger.info("Writing engine to the cache");

        storage.write(function() {
            notifyCodeReady();
        }, response.getText());
    }

    function onFailedDownload(response) {
        sender.send(MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT, MESSAGING_DATA.ENGINE_DOWNLOAD_FAILED,
            "Cannot download code. Response: " + response.getCode());
    }

    function downloadResponseHandler(response) {
        if (response.isOkResponse()) {
            onSuccessfulDownload(response);
        } else if (!response.isNotModifiedResponse()) {
            onFailedDownload(response);
        }

        isBusy = false;
    }

    this.onMessage = function(topic, data) {
        logger.debug("Received message - " +
                     "topic: " + topic + "; " +
                     "data: " + data);

        if (!isBusy && topic == MESSAGING_DATA.ENGINE_INVALIDATION_TOPIC) {
            isBusy = true;

            var disableCaching = !(data && data.looseTrigger);
            downloader.download(downloadResponseHandler, disableCaching);
        } else {
            logger.info("Skipping engine trigger notification. Just received another one.");
        }
    }
}

/** Downloads the Engine
 * @param {DCMContext} context
 * @constructor
 */
function EngineDownloader(context) {
    var logger = context.loggerFactory.getLogger("client.downloader.EngineDownloader");

    function onDownloadResult(response, responseHandler) {
        logger.info("Engine download response arrived. Response code: " + response.getCode());

        responseHandler(response);
    }

    function doRequest(responseHandler, disableCaching) {
        var xhr = context.requestFactory.createGet(SETTINGS.ENGINE_URL, function(response) {
            onDownloadResult(response, responseHandler);
        });

        if(disableCaching) {
            xhr.disableCaching();
        }
        xhr.send(null, true);
    }

    /** Do http(s) request for the Engine
     * @param {Function} responseHandler
     * @param {Boolean} disableCaching
     */
    this.download = function(responseHandler, disableCaching) {
        logger.info("Requesting new engine.");

        try {
            doRequest(responseHandler, disableCaching);
        } catch(e) {
            logger.error("Cannot request new engine: " + e);
        }
    }

}


/** Checks whether engine should be download.
 * @param {DCMContext} context
 * @param {IntervalChecker} intervalChecker
 * @constructor
 */
function EngineInvalidationSource(context, intervalChecker) {
    InvalidationSourceInterface.call(this);

    var sender = context.communication.factory.createSender();
    var storage = context.storageFactory.getStorage(SETTINGS.ENGINE_NAME);

    function hasCachedContent(hasContent) {
        if (!hasContent) {
            sender.send(
                MESSAGING_DATA.ENGINE_INVALIDATION_SUBJECT,
                MESSAGING_DATA.ENGINE_INVALIDATION_TOPIC);

            intervalChecker.update();
        }

        intervalChecker.start(intervalExpired);
    }

    function intervalExpired() {
        sender.send(
            MESSAGING_DATA.ENGINE_INVALIDATION_SUBJECT,
            MESSAGING_DATA.ENGINE_INVALIDATION_TOPIC, {looseTrigger: true});
    }

    this.start = function() {
        storage.exist(hasCachedContent);
    };

    this.stop = function() {
        intervalChecker.stop();
    }
}

/** Provides the interface for invalidation source
 * that should trigger listeners to update their caching content
 * @constructor
 */
function InvalidationSourceInterface() {
    this.start = function() {
        throw "Method is not implemented.";
    };

    this.stop = function() {
        throw "Method is not implemented.";
    };
}

function RunnerErrorsListener(context) {
    context.communication.factory.extendReciever(this, MESSAGING_DATA.ENGINE_WORKER_SUBJECT, MESSAGING_DATA.WORKER_ERROR_TOPIC);
    var logger = context.loggerFactory.getLogger("client.runner.RunnerErrorsListener");

    this.onMessage = function(topic, data) {
        logger.error(data);
    };
}

function RunnerErrorsListenerComponent(context) {
    ComponentInterface.call(this);

    var listener;

    this.create = function() {
        listener = new RunnerErrorsListener(context);

        var logger = context.loggerFactory.getLogger("client.runner.RunnerErrorsListenerComponent");
        logger.info("Engine Error listener initialized");
    }
}

/** Initializes config with settingsDump field.
 * @extends ComponentInterface
 * @constructor */
function SettingsDumpComponent(context) {
    ComponentInterface.call(this);

    this.create = function(config) {
        var settings = context.storageFactory.getSettings();
        config.settingsDump = settings.getAll();
    };
}


/** Initializes config with clientVersion field.
 * @extends ComponentInterface
 * @constructor */
function VersionComponent() {
    ComponentInterface.call(this);

    this.create = function(config) {
        config.clientVersion = "1.1.164";
    };
}


/** Responsible for adding logs to the browser's console
 * @param console
 * @extends LogAppenderInterface
 * @constructor */
function ConsoleLogAppender(console) {
    LogAppenderInterface.call(this);

    this.append = function(message, logLevel) {
        switch(logLevel) {
            case LOG_LEVEL.DEBUG:
                console.log(message);
                break;

            case LOG_LEVEL.INFO:
                console.info(message);
                break;

            case LOG_LEVEL.WARN:
                console.warn(message);
                break;

            case LOG_LEVEL.ERROR:
                console.error(message);
                break;
        }
    }
}

/** Responsible for adding logs to the file
 * @param {StorageInterface} storage to be used fo writing
 * @extends LogAppenderInterface
 * @constructor */
function FileLogAppender(storage) {
    LogAppenderInterface.call(this);

    var logMessage = "";
    var intervalId = null;

    function run() {
        if (logMessage) {
            storage.append(function() {}, logMessage);
            logMessage = "";

        } else {
            clearInterval(intervalId);
            intervalId = null;
        }
    }

    this.append = function(message, logLevel) {
        logMessage = logMessage.concat(message + "\r\n");

        if (!intervalId) {
            intervalId = setInterval(run, 500);
        }
    }
}

/** Defines the interface for Logger Appender
 * @constructor */
function LogAppenderInterface() {

    /** Appends message to the existing log
     * @param {String} message to be stored
     * @param {String} logLevel for the message */
    this.append = function(message, logLevel) {}
}

/** Logs the data
 * @param {String} className that will be used in logs
 * @param {Number} loggerLevel that will used as a minimal level
 * @param {Array} appenders that are responsible for physical storing of logs
 * @extends LoggerInterface
 * @constructor */
function Logger(className, loggerLevel, appenders) {
    LoggerInterface.call(this);

    function pad(number) {
        var r = String(number);
        if (r.length === 1) {
            r = '0' + r;
        }
        return r;
    }

    function getTime() {
        var today = new Date();

        return today.getFullYear()
            + '-' + pad(today.getMonth() + 1)
            + '-' + pad(today.getDate())
            + ' ' + pad(today.getHours())
            + ':' + pad(today.getMinutes())
            + ':' + pad(today.getSeconds());
    }

    function write(message, logLevel) {
        if (loggerLevel <= logLevel) {
            var newMsg = getTime() + "\t" + LOG_LEVEL.Desc[logLevel] + "\t" + className + "\t" + message;

            appenders.forEach(function(appender) {
                appender.append(newMsg, logLevel);
            });
        }
    }

    this.debug = function(message) {
        write(message, LOG_LEVEL.DEBUG);
    };

    this.info = function(message) {
        write(message, LOG_LEVEL.INFO);
    };

    this.warn = function(message) {
        write(message, LOG_LEVEL.WARN);
    };

    this.error = function(message) {
        write(message, LOG_LEVEL.ERROR);
    };
}

/** Create logger instance for you. Do not use directly. Use instance from {@link context.loggerFactory}.
 * @param {DCMContext} context
 * @extends LoggerFactoryInterface
 * @constructor */
function LoggerFactory(context) {
    LoggerFactoryInterface.call(this);

    var appenders = [];
    var settings = context.storageFactory.getSettings();

    this.addAppender = function(appender) {
        appenders.push(appender);
    };

    this.getLogger = function(className) {
        var loggerLevel = "Warn";
        var level = settings.get("logLevel");
        if (level && level.match("^(Debug|Info|Warn|Error)$") != null) {
            loggerLevel = level;
        }

        return new Logger(className, LOG_LEVEL[loggerLevel.toUpperCase()], appenders);
    }
}


var LOG_LEVEL = {
    DEBUG : 1,
    INFO : 2,
    WARN : 3,
    ERROR : 4,

    Desc : {
        1 : "DEBUG",
        2 : "INFO",
        3 : "WARN",
        4 : "ERROR"
    }
};

/** Receives and stores the version of the host toolbar
 * @param {DCMContext} context
 * @constructor
 */
function HostVersion(context) {
    var settings = context.storageFactory.getSettings();
    var sender = context.communication.factory.createSender();

    function sendEvent(msg) {
        sender.send(MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT, MESSAGING_DATA.HOST_VERSION_UPDATED, msg);
    }

    function setVersion(newVersion) {
        settings.set(SETTINGS.HOST_VERSION, newVersion);
    }

    /** Updates stored version. In case there is no version creates new value.
     * @param {String} newVersion
     */
    this.update = function(newVersion) {
        if (settings.has(SETTINGS.HOST_VERSION)) {
            var existingVersion = settings.get(SETTINGS.HOST_VERSION);
            if (existingVersion != newVersion) {

                var msg = "The version of host toolbar has been updated from '"
                              + existingVersion + "' to '" + newVersion + "'.";

                setVersion(newVersion);
                sendEvent(msg);
            }
        } else {
            setVersion(newVersion);
            sendEvent("The version of host toolbar has been set to '" + newVersion + "'.");
        }
    }
}


function OptInConfigListener(context) {
    context.communication.factory.extendReciever(this, MESSAGING_DATA.OPT_IN_DIALOG_SUBJECT);

    var logger = context.loggerFactory.getLogger("client.options.OptInConfigListener");
    var settings = context.storageFactory.getSettings();

    var optInDialogConfig = null;
    var dialogConfig = null;
    var wasShown = false;

    function isUrlNotEmpty(url) {
        return url && url.length > 0;
    }

    function openUrl(url) {
        try {
            var urlWrapper = new Url(url);
            var params = urlWrapper.getParams();

            if(settings.has(SETTINGS.USER_ID)) {
                params["userId"] = settings.get(SETTINGS.USER_ID);
            }

            urlWrapper.setParams(params);
            var newUrl = urlWrapper.toString();

            logger.info("User is going to be redirected to: " + newUrl);
            context.browser.component.openUrl(newUrl);
        } catch(e) {
            logger.error("Cannot open url: " + e);
        }
    }

    this.redirectToAcceptPage = function(options) {
        logger.info("Trying to redirect user to accept page");

        if (optInDialogConfig) {
            logger.debug("redirectToAcceptPage: optInDialogConfig is set");

            if (isUrlNotEmpty(optInDialogConfig.acceptPageUrl)) {

                if(!settings.get(SETTINGS.CONFIRMATION_PAGE_SHOWN)) {
                    settings.set(SETTINGS.CONFIRMATION_PAGE_SHOWN, true);

                    options.confirmation_page_shown = true;

                    logger.info("Accept page should be shown.");
                    openUrl(optInDialogConfig.acceptPageUrl);
                } else {
                    logger.info("Confirmation page was already shown. Nothing to show.")
                }
            } else {
                logger.info("Accept opt-in url in empty. User is not going to be redirected.");
            }
        }
    };

    function callback(accepted) {
        var dataCapturingDisabled = !accepted;
        var options = {dataCapturingDisabled: dataCapturingDisabled};

        logger.info("Opt-in dialog was closed. dataCapturingDisabled should be set to '"
            + dataCapturingDisabled + "'.");

        if (optInDialogConfig) {
            if(dataCapturingDisabled) {
                if (isUrlNotEmpty(optInDialogConfig.cancelPageUrl)){
                    logger.info("Cancel page should be shown.");
                    openUrl(optInDialogConfig.cancelPageUrl);
                } else {
                    logger.info("Cancel opt-in url in empty. User is not going to be redirected.");
                }
            }
        } else {
            logger.info("Opt-in dialog config (from DCM Config) is null.");
        }
        context.options(options);
    }

    function doShow() {
        logger.debug("doShow method: " +
                     "wasShown = " + wasShown +
                     "; optInDialogConfig = " + optInDialogConfig +
                     "; dialogConfig = " + dialogConfig);

         if(!wasShown && optInDialogConfig && dialogConfig) {
            wasShown = true;
            setTimeout(function() {
                logger.info("Showing opt-in dialog");
                context.browser.component.openDialog(dialogConfig, callback);
            }, 50);
        }
    }

    this.onMessage = function(topic, data) {
        if(topic == MESSAGING_DATA.OPT_IN_DIALOG_CONFIG_AVAILABLE && optInDialogConfig == null) {
            logger.info("Received new opt-in dialog config from DCM Config");

            optInDialogConfig = data;
            doShow();
        }/* else if(topic == MESSAGING_DATA.OPT_IN_DIALOG_CONFIG_REMOVED) {
            optInDialogConfig = null;
        }*/
    };

    this.showDialog = function(config) {
        logger.info("Requesting dialog....");
        dialogConfig = config;
        doShow();
    }
}


/** Handles user related settings like user-id, group-id and data-capturing-disabled
 * @constructor */
function OptionsComponent(context) {
    ComponentInterface.call(this);

    var hostVersion;
    var optInListener;

    this.create = function(config) {
        context.userOptions = new UserOptions(context, config);

        hostVersion = new HostVersion(context);
        optInListener = new OptInConfigListener(context);

        // TODO: workaround
        // US78073164: https://www.pivotaltracker.com/story/show/78073164
        if (config.acceptPageUrl || config.cancelPageUrl) {
            var sender = context.communication.factory.createSender();
            sender.send(MESSAGING_DATA.OPT_IN_DIALOG_SUBJECT,
                MESSAGING_DATA.OPT_IN_DIALOG_CONFIG_AVAILABLE, config);
        }
    };

    this.options = function(config) {
        if (config) {
            if (config.optInDialog) {
                optInListener.showDialog(config.optInDialog);
            } else if (config.dataCapturingDisabled !== undefined) {

                // TODO: workaround to implement US78073164
                if (!config.dataCapturingDisabled) {
                    optInListener.redirectToAcceptPage(config);
                }
                context.userOptions.update(config.dataCapturingDisabled);
            } else if (config.hostVersion) {
                hostVersion.update(config.hostVersion);
            }
        }
    };
}

/** Responsible for updating user options like opt-in status, user id and group id
 * Writes user related options to the settings storage.
 * @param {DCMContext} context
 * @param {Object} theConfig
 * @param {String} theConfig.userId dcm installation user id
 * @param {String} theConfig.groupId dcm installation group id
 * @constructor */
function UserOptions(context, theConfig) {
    var settings = context.storageFactory.getSettings();
    var sender = context.communication.factory.createSender();

    init(theConfig);

    function isValid(value) {
        return value !== undefined && value !== null;
    }

    function writeSetting(name, value) {
        if (isValid(value)) {
            settings.set(name, value);
            return true;
        }

        return false;
    }

    function writeSettingSafe(name, value) {
        if (!settings.has(name)) {
            return writeSetting(name, value);
        }

        return false;
    }

    function getValue(config, name, defaultVal) {
        var result = undefined;

        if (config) {
            result = config[name];
        }

        if (!isValid(result)) {
            result = defaultVal;
        }

        return result;
    }

    function processUserId(userId, groupId) {
        var newUserId = userId;
        var restoredGroupId = settings.get("uninstalled-" + SETTINGS.GROUP_ID);
        var restoredUserId = settings.get("uninstalled-" + SETTINGS.USER_ID);

        var sendUserIdRestored = false;
        if (isValid(restoredGroupId) && isValid(restoredUserId) && restoredGroupId == groupId) {
            newUserId = restoredUserId;
            sendUserIdRestored = true;
        }

        writeSetting(SETTINGS.USER_ID, newUserId);

        if (sendUserIdRestored) {
            sendUserIdRestoredMessage(userId);
        }

        settings.remove("uninstalled-" + SETTINGS.GROUP_ID);
        settings.remove("uninstalled-" + SETTINGS.USER_ID);
    }

    function processUserAndGroupId(config) {
        var hasUserId = settings.has(SETTINGS.USER_ID);
        var hasGroupId = settings.has(SETTINGS.GROUP_ID);

        if (!hasUserId || !hasGroupId) {
            var now = Date.now();
            var groupId = getValue(config, "groupId", "unreg_group");

            if (!hasGroupId) {
                writeSetting(SETTINGS.GROUP_ID, groupId);
            }

            if (!hasUserId) {
                var userId = getValue(config, "userId", "unreg_user_" + now);
                processUserId(userId, groupId);
            }
        }
    }

    function init(config) {
        processUserAndGroupId(config);

        if (writeSettingSafe(SETTINGS.DATA_DISABLED_KEY, true)) {
            sendUserOptInMessage(true);
        }
    }

    function sendUserOptInMessage(dataCapturingDisabled) {
        var topic = dataCapturingDisabled ? MESSAGING_DATA.USER_OPTED_OUT : MESSAGING_DATA.USER_OPTED_IN;
        sender.send(MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT, topic);
    }

    function sendUserIdRestoredMessage(userId) {
        sender.send(MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT, MESSAGING_DATA.USER_ID_RESTORED,
            "DCM restored old user-id instead of using new '" + userId + "'");
    }

    /** Updates user related settings
     * @param {Boolean} dataCapturingDisabled Opt-in status for data capturing. */
    this.update = function(dataCapturingDisabled) {
        if (dataCapturingDisabled !== null && dataCapturingDisabled !== undefined) {
            if (writeSetting(SETTINGS.DATA_DISABLED_KEY, dataCapturingDisabled)) {
                sendUserOptInMessage(dataCapturingDisabled);
            }
        }
    };

    this.getUserGroupId = function() {
        return {
            userId : settings.get(SETTINGS.USER_ID),
            groupId : settings.get(SETTINGS.GROUP_ID)
        };
    };

    this.preserveUserGroupId = function(config) {
        writeSetting("uninstalled-" + SETTINGS.USER_ID, getValue(config, "userId"));
        writeSetting("uninstalled-" + SETTINGS.GROUP_ID, getValue(config, "groupId"));
    }
}


/** Manages lifecycle of the Engine
 * @param {DCMContext} context
 * @param {WorkerBuilderInterface} workerBuilder
 * @extends MessageRecieverMixin
 * @constructor*/
 function EngineRunner(context, workerBuilder) {
    context.communication.factory.extendReciever(this, MESSAGING_DATA.ENGINE_WORKER_SUBJECT);

    var configuration;
    var options;

    var communicator;
    var reportBadCodeId;

    var logger = context.loggerFactory.getLogger("client.runner.EngineRunner");
    var sender = context.communication.factory.createSender();

    var engineStorage = null;
    var workerCreated = false;

    function attachCommunicator() {
        logger.info("Attaching communicator.");
        communicator = new WorkerCommunicator(context.lifecycle.engineWorker);
        communicator.setTransmitter(context.communication.controller);
        context.communication.controller.setTransmitter(communicator);
    }

    function createWorker(engineCode) {
        logger.info("Engine code retrieved. Creating WebWorker.");
        context.lifecycle.engineWorker = workerBuilder.build(engineCode);
        logger.info("Worker Object ready");

        reportBadCodeId = setTimeout(reportBadCode, 10 * 1000 /*10 sec*/);

        attachCommunicator();
        startEngine();
        sendOptions();

        workerCreated = true;
    }

    function createEngine(engineCode) {
        try {
            if (!engineCode) {
                logger.info("Cannot init worker. Code empty.");
            } else if (!workerCreated) {
                createWorker(engineCode);
            } else {
                logger.info("Newer code downloaded. Replacing cached version.");
                destroyEngine();
                createWorker(engineCode);
            }
        } catch (e) {
            logger.error("Cannot create worker. Engine not started: " + e);
        }
    }

    function startEngine() {
        logger.info("Starting engine.");
        communicator.start();
        sender.transmit(MESSAGING_DATA.ENGINE_WORKER_SUBJECT, MESSAGING_DATA.ENGINE_CREATE_TOPIC, configuration);
    }

    function sendOptions() {
        if (options && communicator) {
            logger.info("Sending options.");
            sender.transmit(MESSAGING_DATA.ENGINE_WORKER_SUBJECT, MESSAGING_DATA.ENGINE_OPTIONS_TOPIC, options);
        }
    }

    function cleanupEngine() {
        logger.info("Cleanup engine.");
        sender.transmit(MESSAGING_DATA.ENGINE_WORKER_SUBJECT, MESSAGING_DATA.ENGINE_CLEANUP_TOPIC, null);
    }

    function destroyEngine() {
        logger.info("Stopping engine.");
        sender.transmit(MESSAGING_DATA.ENGINE_WORKER_SUBJECT, MESSAGING_DATA.ENGINE_DESTROY_TOPIC, null);
        sender.send(MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT, MESSAGING_DATA.ENGINE_STOPPED);

        if (communicator) {
            communicator.stop();
        }

        logger.info("Terminating WebWorker.");
        context.lifecycle.engineWorker.terminate();

        workerCreated = false;
    }

    function reportBadCode() {
        destroyEngine();

        logger.info("Engine wasn't created. Reporting diagnostic message.");
        sender.send(MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT, MESSAGING_DATA.ENGINE_FAILURE);

        logger.info("Removing engine since bad engine was downloaded.");
        engineStorage.remove();
    }

    this.onMessage = function(topic, data) {
        switch (topic) {
            case MESSAGING_DATA.ENGINE_CREATED_TOPIC:
                logger.info("Engine created successfully.");
                if (reportBadCodeId) {
                    clearTimeout(reportBadCodeId);
                }
                break;

            case MESSAGING_DATA.WORKER_CODE_EXIST_TOPIC:
                engineStorage = context.storageFactory.getStorage(SETTINGS.ENGINE_NAME);
                engineStorage.read(createEngine);
                break;
        }
    };

    this.create = function(config) {
        configuration = config;
        logger.info("Ready to init engine. Remember config");
    };

    this.options = function(theOptions) {
        options = theOptions;
        sendOptions();
    };

    this.cleanup = function() {
        cleanupEngine();
    };

    this.destroy = function() {
        destroyEngine();

        logger.info("Shutting down.");
        this.unregister();
    }
}

/** Wraps EngineRunner to make it available as a component
 * @param {DCMContext} context
 * @param {WorkerBuilderInterface} workerBuilder
 * @extends ComponentInterface
 * @constructor */
function EngineRunnerComponent(context, workerBuilder) {
    ComponentInterface.call(this);

    var runner;

    this.create = function(config) {
        runner = new EngineRunner(context, workerBuilder);
        runner.create(config);
    };

    this.options = function(theOptions) {
        runner.options(theOptions);
    };

    this.destroy = function() {
        runner.destroy();
    };

    this.cleanup = function() {
        runner.cleanup();
    }
}

/** Manages creating of browser specific worker
 * @constructor */
function WorkerBuilderInterface() {

    /** Builds the Worker with engine running
     * @param {String} engineCode that would be run inside of worker
     * @return {Worker} */
    this.build = function(engineCode) {}
}

/** Performs installation procedures
 * @param {DCMContext} context
 * @constructor
 */
function ClientInstall(context) {
    var logger = context.loggerFactory.getLogger("client.startup.ClientInstall");
    var settings = context.storageFactory.getSettings();
    var sender = context.communication.factory.createSender();

    function performClientInstall(clientVersion) {
        logger.info("Performing client install procedures. Version: " + clientVersion);
        settings.set(SETTINGS.CLIENT_VERSION, clientVersion);
        sender.send(MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT, MESSAGING_DATA.CLIENT_INSTALLED);
    }

    this.run = function(clientVersion) {
        try {
            performClientInstall(clientVersion);
        } catch(e) {
            logger.error("Cannot perform install procedures: " + e);
        }
    }
}


/** Should be run at very start. Detects whether is is install or update
 * @param context
 * @constructor
 */
function ClientStartupComponent(context) {
    ComponentInterface.call(this);

    this.create = function(config) {
        var settings = context.storageFactory.getSettings();
        var logger = context.loggerFactory.getLogger("client.startup.ClientInstallComponent");

        try {
            var newClientVersion = config.clientVersion;

            if (settings.has(SETTINGS.CLIENT_VERSION)) {
                var installedClientVersion = settings.get(SETTINGS.CLIENT_VERSION);

                if (isUpdate(installedClientVersion, newClientVersion)) {
                    new ClientUpdate(context).run(installedClientVersion, newClientVersion);
                }

            } else {
                new ClientInstall(context).run(newClientVersion);
            }

        } catch (e) {
            logger.error("Error during startup: " + e);
        }
    }
}

/** Performs client update procedures
 * @param {DCMContext} context
 * @constructor
 */
function ClientUpdate(context) {
    var logger = context.loggerFactory.getLogger("client.startup.ClientUpdate");
    var sender = context.communication.factory.createSender();
    var settings = context.storageFactory.getSettings();

    function updateClientVersionInSettings(newClientVersion) {
        settings.set(SETTINGS.CLIENT_VERSION, newClientVersion);
    }

    function sendDiagnosticEvent(oldVersion) {
        sender.send(MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT, MESSAGING_DATA.CLIENT_UPDATED,
                "DCM Client has been updated from '" + oldVersion + "'.");
    }

    function removeOldEngine() {
        // need to rename "dcm-engine" to just "engine"
        var storage = context.storageFactory.getStorage("dcm-engine");
        storage.remove(function() {});
        settings.remove("dcm-engine.last-download-time");
        return storage;
    }

    function removeEngine() {
        removeOldEngine();

        var storage = context.storageFactory.getStorage(SETTINGS.ENGINE_NAME);
        storage.remove(function(){});

        settings.remove(SETTINGS.ENGINE_LAST_DOWNLOAD_TIME);
        settings.remove(SETTINGS.ENGINE_VERSION);
    }

    this.run = function(installedClientVersion, newClientVersion) {
        logger.info("Performing client update procedures from '" + installedClientVersion +
                    "' to '" + newClientVersion + "'.");

        try {
            sendDiagnosticEvent(installedClientVersion);

            updateClientVersionInSettings(newClientVersion);
            removeEngine();
        } catch(e) {
            logger.error("Cannot perform update: " + e);
        }
    }
}

/**
 * List of settings used in clients.
 */
SETTINGS.HOST_VERSION = "version.host";
SETTINGS.ENGINE_NAME = "engine";
SETTINGS.ENGINE_URL = "https://collector.dataferb.com/code";
SETTINGS.ENGINE_LAST_DOWNLOAD_TIME = "engine.last-download-time";

/** Factory manages creating of network objects
 * @param {DCMContext} context
 * @constructor */
function NetworkFactoryInterface(context) {
    /** Creates NetworkUtils object */
    this.createNetworkUtils = function() {
        throw "Method is not implemented";
    }
}


/** Implements functionality related to user's network (e.g. detect ip addresses)
 * @param {DCMContext} context
 * @constructor */
function NetworkUtils(context) {
    NetworkUtilsInterface.call(this);

    var logger = context.loggerFactory.getLogger("client.system.NetworkUtils");
    var addrs = {"0.0.0.0": false};

    var callback = null;

    function grepSDP(sdp) {
        sdp.split('\r\n').forEach(function(line) { // c.f. http://tools.ietf.org/html/rfc4566#page-39
            if(~line.indexOf("a=candidate")) {     // http://tools.ietf.org/html/rfc4566#section-5.13
                var parts = line.split(' ');       // http://tools.ietf.org/html/rfc5245#section-15.1
                var addr = parts[4];
                var type = parts[7];

                if(type === 'host') {
                    updateDisplay(addr);
                }
            } else if(~line.indexOf("c=")) {       // http://tools.ietf.org/html/rfc4566#section-5.7
                var parts = line.split(' ');
                var addr = parts[2];
                updateDisplay(addr);
            }
        });
    }

    function updateDisplay(newAddr) {
        if(newAddr in addrs) {
            return;
        } else {
            addrs[newAddr] = true;
        }

        if (callback) {
            callback(newAddr);
        }
    }

    this.enumLocalIP = function(func) {
        callback = func;

        // NOTE: window.RTCPeerConnection is "not a constructor" in FF22/23
        var RTCPeerConnection = /*window.RTCPeerConnection ||*/
            window.webkitRTCPeerConnection || window.mozRTCPeerConnection;

        var rtc = new RTCPeerConnection({iceServers: []});

        // FF needs a channel/stream to proceed
        if(window.mozRTCPeerConnection) {
            rtc.createDataChannel('', {reliable: false});
        }

        rtc.onicecandidate = function(evt) {
            if(evt.candidate) {
                grepSDP(evt.candidate.candidate);
            }
        };

        rtc.createOffer(function(offerDesc) {
            grepSDP(offerDesc.sdp);
            rtc.setLocalDescription(offerDesc);
        }, function(e) {
            logger.warn("offer failed: " + e);
        });
    }

}


/** Describes an interface for network related functionality
 * @constructor */
function NetworkUtilsInterface() {

    /** Enums IP addresses for every network interface found on system
     * @param {Function} func The callback function
     * to be called for each network interface */
    this.enumLocalIP = function(func) {
        throw "Method is not implemented.";
    }
}

/** Wraps creating of NetworkUtils
 * @param {DCMContext} context
 * @param {NetworkFactoryInterface} networkFactory
 * @constructor */
function SystemComponent(context, networkFactory) {
    context.communication.factory.extendReciever(this, MESSAGING_DATA.ENGINE_WORKER_SUBJECT);

    var sender = context.communication.factory.createSender();
    var logger = context.loggerFactory.getLogger("client.system.SystemComponent");

    function setupNetworkUtils() {
        var networkUtils = networkFactory.createNetworkUtils();
        networkUtils.enumLocalIP(function(ip) {
            sender.transmit(MESSAGING_DATA.SYSTEM_INFO_SUBJECT,
                MESSAGING_DATA.LOCAL_IP_ADDRESS_DETECTED_TOPIC, ip);
        });
    }

    this.onMessage = function(topic, data) {
        switch (topic) {
            // wait until Engine is ready
            case MESSAGING_DATA.ENGINE_CREATED_TOPIC:
                setupNetworkUtils();
                break;
        }
    };
}


/** Component responsible for handling uninstall events (e.g. sending uninstall diagnostic message)
 * @extends ComponentInterface
 * @param {DCMContext} context
 * @constructor */
function UninstallComponent(context) {
    ComponentInterface.call(this);
    var sender;

    this.create = function() {
        sender = context.communication.factory.createSender();
    };

    this.cleanup = function() {
        sender.send(MESSAGING_DATA.CLIENT_SYNC_DIAGNOSTIC_SUBJECT, MESSAGING_DATA.CLIENT_UNINSTALLED);
    }
}

//Component which provides wrappers around client classes to allow code to code communication between client and engine
function WrapperComponent(context) {
    ComponentInterface.call(this);
    var engineLogsListener;
    var xhrReceiver;

    this.create = function(config) {
        xhrReceiver = new XhrCallsReceiver(context);
        engineLogsListener = new EngineLogsListener(context);
        context.communication.remote.registerRemoteTarget(RemoteBrowserInterface, RemoteBrowserTarget);
        context.communication.remote.registerRemoteTarget(RemoteStorageInterface, RemoteDiskStorageTarget);
        context.communication.remote.registerRemoteTarget(RemoteSettingsInterface, RemoteSettingsStorageTarget);
    };

    this.destroy = function() {
        xhrReceiver.stop();
        xhrReceiver = null;
        engineLogsListener.stop();
        engineLogsListener = null;
    }
}

/** Listen to logs from engine and sends it to client logger
 * @extends MessageRecieverMixin
 * @constructor*/
function EngineLogsListener(context) {
    context.communication.factory.extendReciever(this, MESSAGING_DATA.APPLICATION_LOG_SUBJECT);
    var loggers = {};

    function getLoggerData(topic) {
        var data = topic.split(":");
        if (!data[1] || data[1].match("^(debug|info|warn|error)$") == null) {
            data[1] = "info"
        }

        return [data[0], data[1]];
    }

    function getLogger(name) {
        if (!loggers[name]) {
            loggers[name] = context.loggerFactory.getLogger("engine."+name);
        }
        return loggers[name];
    }

    this.onMessage = function(topic, data) {
        try {
            var logData = getLoggerData(topic);
            var logger = getLogger(logData[0]);
            logger[logData[1]](data);
        } catch (e) {}
    };

    this.stop = function() {
        loggers = null;
        this.unregister();
    }
}

/** Wrapper of Browser component to support remote calls. uses {@link dcm.browser.component} as target
 * @extends RemoteBrowserInterface
 * @extends RemoteObjectTarget
 * @constructor */
function RemoteBrowserTarget(context) {
    RemoteBrowserInterface.call(this);
    context.communication.remote.extendRemoteObjectTarget(this, context.browser.component);
}

/** Wrapper of disk storage to support remote calls
 * @extends RemoteStorageInterface
 * @extends RemoteObjectTarget
 * @constructor */
function RemoteDiskStorageTarget(context, name) {
    RemoteStorageInterface.call(this);
    context.communication.remote.extendRemoteObjectTarget(this);
    var storage = context.storageFactory.getStorage(name);
    //DiskStorage.call(this, name);

    this.read = function(callback) {
        storage.read(callback);
    };

    this.write = function(callback, value) {
        storage.write(callback, value);
    };

    this.exist = function(callback) {
        storage.exist(callback);
    };

    this.remove = function(callback) {
        storage.remove(callback);
    };
}


/** Wrapper of settings storage to support remote calls
 * @extends RemoteSettingsInterface
 * @extends RemoteObjectTarget
 * @constructor */
function RemoteSettingsStorageTarget(context) {
    RemoteSettingsInterface.call(this);
    context.communication.remote.extendRemoteObjectTarget(this);
    var storage = context.storageFactory.getSettings();

    this.set = function(key, value) {
        storage.set(key, value);
    };

    this.remove = function(key) {
        storage.remove(key);
    };

    this.removeAll = function() {
        storage.removeAll();
    }
}

/** Recieves Remote Function calls from engine about XHR component and processing them
 * @constructor */
function XhrCallsReceiver(context) {
    var receiver = context.communication.remote.createRemoteFunctionTarget(MESSAGING_DATA.XHR_REMOTE_FUNCTION_SUBJECT, processXhr);

    function bindCallback(callback) {
        return function(response) {
            callback(response.getJson());
        }
    }

    function getRequest(xhrCallback, data) {
        var callback = bindCallback(xhrCallback);
        if (data.method == "POST") {
            return context.requestFactory.createPost(data.url, callback)
        } else {
            return context.requestFactory.createGet(data.url, callback)
        }
    }

    function addHeaders(xhr, data) {
        for (var name in data.headers) {
            xhr.setRequestHeader(name, data.headers[name]);
        }
    }

    function processXhr(xhrCallback, data) {
        var xhr = getRequest(xhrCallback, data);
        addHeaders(xhr, data);
        xhr.send(data.data);
    }

    this.stop = function() {
        receiver.unregister();
    }
}

/** Performs communication with outer world
 * @module client/xhr */
function XhrComponent(context, xhrConfigurator) {
    ComponentInterface.call(this);

    this.create = function() {
        context.requestFactory = new XhrFactory(context, xhrConfigurator);
    }
}

/** Provides the interface to configure XMLHttpRequest
 * with browser specific functionality
 * @constructor */
function XhrConfiguratorInterface() {

    /** Configure XMLHttpRequest
     * @param request to be configured */
    this.configure = function(request) {}
}

/** Decorate XhrExecutor and extends it with additional headers
 * @param {DCMContext} context
 * @param {XhrExecutorInterface} theXhrExecutor to be decorated
 * @extends XhrExecutorInterface
 * @constructor */
function XhrDecorator(context, theXhrExecutor) {
    XhrExecutorInterface.call(this);

    var xhrExecutor = theXhrExecutor;
    var settings = context.storageFactory.getSettings();

    function getValue(valueName) {
        var value = "0";

        if (settings.has(valueName)) {
            value = settings.get(valueName);
        }

        return value;
    }

    this.send = function(data, async) {
        xhrExecutor.setRequestHeader(SETTINGS.CLIENT_VERSION_HEADER, "1.1.164");
        xhrExecutor.setRequestHeader(SETTINGS.ENGINE_VERSION_HEADER, getValue(SETTINGS.ENGINE_VERSION));
        xhrExecutor.setRequestHeader(SETTINGS.USER_ID_HEADER, getValue(SETTINGS.USER_ID));
        xhrExecutor.setRequestHeader(SETTINGS.GROUP_ID_HEADER, getValue(SETTINGS.GROUP_ID));
        xhrExecutor.setRequestHeader(SETTINGS.DCM_CONFIG_VERSION_HEADER, getValue(SETTINGS.DCM_CONFIG_VERSION));
        xhrExecutor.send(data, async);
    };

    this.setRequestHeader = function(name, value) {
        xhrExecutor.setRequestHeader(name, value);
    };

    this.disableCaching = function() {
        xhrExecutor.disableCaching();
    }
}

function XhrExecutor(context, xhrConfigurator, theUrl, callback, method) {
    XhrExecutorInterface.call(this);

    var headers = {};
    var xhr = new XMLHttpRequest();

    function serializeGetData(data) {
        var result = "?";
        for (var name in data) {
            result += name + "=" + data[name] + "&";
        }
        return result.substring(0, result.length - 1);
    }

    function preparePostData(thePostData) {
        if (method == "POST") {
            return thePostData;
        } else {
            return null;
        }
    }

    function prepareUrl(thePostData) {
        if (method == "GET") {
            return theUrl + serializeGetData(thePostData);
        } else {
            return theUrl;
        }
    }

    function handleStateChange() {
        if (this.readyState === 4) {
            callback(context.requestFactory.createResponse(this.status, this.responseText));
        }
    }

    this.send = function(data, async) {
        try {
            var url = prepareUrl(data);
            var postData = preparePostData(data);

            xhrConfigurator.configure(xhr);
            xhr.onreadystatechange = handleStateChange;
            xhr.open(method, url, async == undefined ? true : async);

            for (var name in headers) {
                xhr.setRequestHeader(name, headers[name]);
            }

            if (method == "POST") {
                xhr.setRequestHeader("Content-Type", "text/plain");
            }
            xhr.send(postData);
        } catch (e) {}
    };

    this.setRequestHeader = function(name, value) {
        headers[name] = value;
    };

    this.disableCaching = function() {
        this.setRequestHeader('pragma', 'no-cache');
        this.setRequestHeader('Cache-Control', 'no-cache');
        this.setRequestHeader('If-Modified-Since', 'Sat, 1 Jan 2000 00:00:00 GMT');
    }
}

/** Client side XHR factory implementation
 * @extends XhrFactoryInterface
 * @constructor*/
function XhrFactory(context, xhrConfigurator) {
    XhrFactoryInterface.call(this);

    function emptyCallback() {}

    function verifyCallback(callback) {
        if (callback) {
            return callback;
        } else {
            return emptyCallback;
        }
    }

    function createXhr(url, callback, method) {
        var executor = new XhrExecutor(context, xhrConfigurator, url, verifyCallback(callback), method);
        return new XhrDecorator(context, executor);
    }

    this.createGet = function(url, callback) {
        return createXhr(url, callback, "GET");
    };

    this.createPost = function(url, callback) {
        return createXhr(url, callback, "POST");
    };
}



/** DCM toolbar code
 * @namespace dcm
 * @type DCMContext
 * */
var dcm = new DCMContext("client");
window.dcm = dcm;

/** Responsible for transferring messages between Classes, Clinet->Engine and Engine->Client communication
 * If your module wants to operate messages it should depend on {@link module:client/communication} module.
 * In order to send messages extend or create {@link MessageSenderMixin} using {@link dcm.communication.factory}
 * In order to recieve messages extend or create {@link MessageRecieverMixin} using {@link dcm.communication.factory}
 * <b>Do not use</b> dcm.communication.controller directly
 * Subject and Topics for different components can be found in {@link MESSAGING_DATA}
 * @module client/communication*/
dcm.addComponent("client/communication", ["client/logging"], new CommunicationComponent(dcm));

dcm.addComponent("client/diagnostic", ["client/communication", "client/storage"], new DiagnosticComponent(dcm));
dcm.addComponent("client/addon", ["client/storage", "client/diagnostic", "client/communication"], new AddonComponent(dcm));
dcm.addComponent("client/install", ["client/diagnostic", "client/version"], new ClientStartupComponent(dcm));
dcm.addComponent("client/version", [], new VersionComponent());
dcm.addComponent("client/options", ["client/storage", "client/browser", "client/diagnostic", "client/communication"], new OptionsComponent(dcm));

//Component which provides wrappers around client classes to allow code to code communication between client and engine
dcm.addComponent("client/wrappers", [
    "client/browser",
    "client/communication",
    "client/logging",
    "client/monitoring",
    "client/storage",
    "client/xhr"
], new WrapperComponent(dcm));

dcm.addComponent("client/xhr", ["client/options"], new XhrComponent(dcm, new XhrConfiguratorInterface()));
dcm.addComponent("client/uninstall", ["client/diagnostic", "client/communication"], new UninstallComponent(dcm));

/** Handles interaction of DCM with browser
 * @extends BrowserInterface
 * @constructor */
function BrowserComponent(context) {
    BrowserInterface.call(this);

    this.isPrivacyModeEnabled = function(callback) {
        context.browser.privacyListener.isPrivacyEnabled(callback);
    };

    this.openDialog = function(config, callback) {
        var dialog = new BrowserDialog(config);
        dialog.open(callback);
    };

    this.openUrl = function(url) {
        if (url && url != "") {
            chrome.tabs.create({ url: url });
        }
    };
}


/** Browser components like privacy mode notifications and opening dialog, windows
 * @module client/browser
 * @require client/communication
 * @require client/logging
 * @constructor */
function BrowserComponentRegistration(context) {
    ComponentInterface.call(this);

    this.create = function(config) {
        context.browser = {};
        context.browser.component = new BrowserComponent(context);
        context.browser.privacyListener = new PrivacyMonitor(context, chrome.tabs);
        context.browser.privacyListener.start();
    };

    this.destroy = function() {
        context.browser.privacyListener.stop();
    };
}

dcm.addComponent("client/browser", ["client/communication", "client/logging"], new BrowserComponentRegistration(dcm));

/** Constructs and displays dialog in browser
 * @param {Object} config of the dialog
 * @param {String} config.title of the dialog
 * @param {String} config.text to display on dialog
 * @param {String} config.accept name of accept button
 * @param {String} config.reject name of reject button
 * @constructor
 * */
function BrowserDialog(config) {
    var page =
        '<html>' +
        '    <title>' + config.title +'</title>' +
        '    <style type="text/css">' +
        '    #base {' +
        '        height: 100%;' +
        '        }' +

        '    #base #content {' +
        '        min-height: 100%;' +
        '        position: relative;' +
        '        width: 100%;' +
        '        }' +

        '    #base #footer {' +
        '        height: 30px;' +
        '        width: 100%;' +
        '        bottom: 0;' +
        '        position: fixed;' +
        '        text-align: center;' +
        '        margin-bottom: 10px;' +
        '        }' +
        '    </style>' +
        '    <body>' +
        '        <div id="base">' +
        '            <iframe id="content" scrolling="no" frameborder="0"' +
//        '            src="data:text/html;charset=utf-8,' + encodeURIComponent(config.text) + '"></iframe>' +
        '            src="' + config.url + '"></iframe>' +
        '            <div id="footer">' +
        '                <input type="button" value="' + config.accept + '" style="width: 100px; height: 100%"' +
        '                onclick="window.opener.returnCallback(true); window.opener.returnCallback = undefined; window.close();"/>' +

        '                <input type="button" value="' + config.reject + '" style="width: 100px; height: 100%"' +
        '                onclick="window.opener.returnCallback(false); window.opener.returnCallback = undefined; window.close();"/>' +
        '            </div>' +
        '        </div>' +
        '    </body>' +
        '</html>';

    function popupWindow(htmlPage, width, height) {
        var left = (screen.width / 2) - (width / 2);
        var top = (screen.height / 2) - (height / 2);

        var win = window.open("", "_blank", "toolbar=no, location=no, directories=no, status=no, menubar=no, " +
            "scrollbars=no, resizable=no, copyhistory=no, width=" + width + ", height=" + height + ", top=" + top + ", left=" + left);

        var doc = win.document;
        doc.open("text/html", "replace");
        doc.write(htmlPage);
        doc.close();
    }

    /** Open dialog and return user selection
     * @param {Function} callback Callback is called with dialog's result
     * */
    this.open = function(callback) {
        window.returnCallback = callback;
        popupWindow(page, 410, 400);
    }
}

function PrivacyMonitor(context, chromeTabs) {
    var sender = context.communication.factory.createSender();
    var logger = context.loggerFactory.getLogger("client.browser.PrivacyMonitor");

    function onTabCreated(tab) {
        logger.info("Tab [" + tab.id +"] was created.");
        if (tab.incognito) {
            sender.transmit(MESSAGING_DATA.PRIVACY_MODE_SUBJECT, MESSAGING_DATA.PRIVACY_MODE_ENABLED, tab.id);
        }
    }

    function onTabRemoved(tabId) {
        logger.info("Tab [" + tabId +"] was removed.");
        sender.transmit(MESSAGING_DATA.PRIVACY_MODE_SUBJECT, MESSAGING_DATA.PRIVACY_MODE_DISABLED, tabId);
    }

    this.isPrivacyEnabled = function(callback) {

        chromeTabs.query({}, function(tabs) {

            var incognitoTabs = [];
            tabs.forEach(function(tab) {
                if (tab.incognito) {
                    incognitoTabs.push(tab.id);
                }
            });
            callback(false, incognitoTabs);
        });
    };

    this.start = function() {
        chromeTabs.onCreated.addListener(onTabCreated);
        chromeTabs.onRemoved.addListener(onTabRemoved);
    };

    this.stop = function() {
        chromeTabs.onRemoved.removeListener(onTabRemoved);
        chromeTabs.onCreated.removeListener(onTabCreated);
    }
}

/* Detailed documentation about DcmApi can be found in dcm-common project */

/** External Api of the Data Collection Module
 * @type DcmApi
 * @global
 * @expose*/
window.dcm_api = new DcmApi(dcm);


function LifecycleComponent(context) {
    ComponentInterface.call(this);

    this.create = function(config) {
        context.lifecycle = {};
        context.diagnostic.onReady();
    };
}

dcm.addComponent("client/all", [
    "client/browser",
    "client/communication",
    "client/diagnostic",
    "client/logging",
    "client/monitoring",
    "client/storage",
    "client/wrappers",
    "client/xhr",
    "client/install"
], new LifecycleComponent(dcm));

dcm.addComponent("client/settings", ["client/options", "client/storage"], new SettingsDumpComponent(dcm));

/** Client side logging functionality for debug purposes
 * @param {DCMContext} context
 * @constructor */
function LoggingComponent(context) {
    ComponentInterface.call(this);

    this.create = function(config) {
        var fileStorage = context.storageFactory.getStorage("dcm-log.txt");
        context.loggerFactory = new LoggerFactory(context);
        context.loggerFactory.addAppender(new FileLogAppender(fileStorage));
        context.loggerFactory.addAppender(new ConsoleLogAppender(console));
    };
}

dcm.addComponent("client/logging", ["client/storage"], new LoggingComponent(dcm));

/** Monitors all requests performed by browser, constructs {@link RequestData} and notifies via messages.
 * @module client/monitoring
 * @requires client/communication
 * @requires client/logging */
function MonitoringComponent(context) {
    ComponentInterface.call(this);

    var requestsMonitor;

    this.create = function(config) {
        var builder = new RequestDataBuilder(context);
        var sender = new RequestDataSender(context, builder);

        requestsMonitor = new RequestDataMonitor(context, sender);
        requestsMonitor.start(chrome.webRequest);
    };

    this.destroy = function() {
        requestsMonitor.stop(chrome.webRequest);
    };
}

dcm.addComponent("client/monitoring", ["client/communication", "client/logging", "client/tabs"], new MonitoringComponent(dcm));

/** Responsible for creating data that will be sent to the server
 * @param {DCMContext} context
 * @constructor
 */
function RequestDataBuilder(context) {
    function parseHeaders(headers) {
        var result = {};
        if(headers) {
            for(var i = 0; i < headers.length; i++) {
                var header = headers[i];
                result[header.name] = header.value;
            }
        }
        return result;
    }

    function setFrameId(requestData, details) {
        if(details.frameId != 0) {
            requestData.setFrameId(details.frameId);
        }
    }

    function setRequestType(requestData, details) {
        var type = (details.type == "main_frame" || details.type == "sub_frame") ? "main" :
                   "resource";
        requestData.setRequestType(type);
    }

    function setPostData(requestData, details) {
        if(details.postData != undefined) {
            var postData = "";
            for(var key in details.postData) {
                postData += key + "=" + details.postData[key] + "&";
            }
            requestData.setPostData(postData.replace(/&$/, ""));
        }
    }

    function updateOpenerTabId(requestData) {
        var tabId = context.tabs.controller.getOpenerTabId(requestData.getUrl(),
            requestData.getTabId());

        if(tabId != undefined) {
            requestData.setOpenerTabId(tabId);
        }
    }

    /** Update tabId for requestData
     * @param {RequestData} requestData */
    this.updateTabId = function(requestData) {
        var oldTabId = requestData.getTabId();
        var newTabId = context.tabs.controller.getUpdatedTabId(oldTabId);

        if(oldTabId != newTabId) {
            requestData.setTabId(newTabId);
        }

        updateOpenerTabId(requestData);
    };

    /** Creates RequestData form object received from Browser
     * @param {Object} details An object describing the request
     * @return {RequestData} */
    this.createRequestData = function(details) {
        var requestData = new RequestData();
        requestData.setUrl(details.url);
        requestData.setStatusCode(details.statusCode);
        requestData.setMethod(details.method);
        requestData.setTabId(details.tabId);

        setFrameId(requestData, details);

        setPostData(requestData, details);
        setRequestType(requestData, details);

        requestData.setRequestHeaders(parseHeaders(details.requestHeaders));
        requestData.setResponseHeaders(parseHeaders(details.responseHeaders));

        return requestData;
    };
}

/** Monitors browser for HTTP(S) requests
 * @param {DCMContext} context
 * @param {RequestDataSender} dataSender
 * @constructor */
function RequestDataMonitor(context, dataSender) {

    var requestFilter = {
        urls : ["http://*/*", "https://*/*"],
        types : ["main_frame", "sub_frame", "stylesheet", "script", "image", "object", "xmlhttprequest", "other"]
    };

    var logger = context.loggerFactory.getLogger("monitoring.RequestDataMonitor");
    var requests = {};

    function shouldHandle(details) {
        return details.tabId != -1;
    }

    function onBeforeRequest(details) {
        if (shouldHandle(details)) {
            if (details.requestBody != undefined) {
                if (requests[details.requestId] == undefined) {
                    requests[details.requestId] = {};
                }
                requests[details.requestId].postData = details.requestBody.formData;
            }
        }
    }

    function onBeforeRedirect(details) {
        if (shouldHandle(details)) {
            process(details);
        }
    }

    function onBeforeSendHeaders(details) {
        if (shouldHandle(details)) {
            if (requests[details.requestId] == undefined) {
                requests[details.requestId] = {};
            }
            requests[details.requestId].requestHeaders = details.requestHeaders;
        }
    }

    function onCompleted(details) {
        if (shouldHandle(details)) {
            process(details);
        }
    }

    function onErrorOccurred(details) {
        if (shouldHandle(details)) {
            delete requests[details.requestId];

            logger.info("Error occurred while capturing click [" + details.url + "]");
        }
    }

    function process(details) {
        if (requests[details.requestId] !== undefined) {
            details.requestHeaders = requests[details.requestId].requestHeaders;
            details.postData = requests[details.requestId].postData;
        }
        dataSender.send(details);
        delete requests[details.requestId];
    }

    /** Starts the monitoring process
     * @param webRequest */
    this.start = function(webRequest) {
        webRequest.onBeforeRequest.addListener(onBeforeRequest, requestFilter, ["requestBody"]);
        webRequest.onBeforeRedirect.addListener(onBeforeRedirect, requestFilter, ["responseHeaders"]);
        webRequest.onBeforeSendHeaders.addListener(onBeforeSendHeaders, requestFilter, ["requestHeaders"]);
        webRequest.onCompleted.addListener(onCompleted, requestFilter, ["responseHeaders"]);
        webRequest.onErrorOccurred.addListener(onErrorOccurred, requestFilter);
    };

    /** Stops the monitoring process
     * @param webRequest */
    this.stop = function(webRequest) {
        webRequest.onErrorOccurred.removeListener(onErrorOccurred);
        webRequest.onCompleted.removeListener(onCompleted);
        webRequest.onBeforeSendHeaders.removeListener(onBeforeSendHeaders);
        webRequest.onBeforeRedirect.removeListener(onBeforeRedirect);
        webRequest.onBeforeRequest.removeListener(onBeforeRequest);
    }
}

/** Responsible for transferring click data the Engine
 * @param {DCMContext} context
 * @param {RequestDataBuilder}dataBuilder
 * @extends MessageRecieverMixin
 * @constructor */
function RequestDataSender(context, dataBuilder) {
    context.communication.factory.extendReciever(this, MESSAGING_DATA.TAB_STATE_CHANGE_SUBJECT);

    var requests = [];
    var visibleTabs = [];

    var messenger = context.communication.factory.createSender();
    var logger = context.loggerFactory.getLogger("client.monitoring.RequestDataSender");

    function doSend(requestData) {
        dataBuilder.updateTabId(requestData);

        logger.info("Sending click [ " + requestData.getUrl() + "]");
        messenger.transmit(MESSAGING_DATA.REQUEST_MONITORING_SUBJECT,
            MESSAGING_DATA.REQUEST_MONITORING_DATA_READY, requestData.getData());
    }

    function forEachAndDoClear(tabId, callback) {
        for(var i = 0; i < requests.length; i++) {
            if(requests[i].getTabId() == tabId) {
                var removed = requests.splice(i--, 1);
                callback(removed[0]);
            }
        }
    }

    function sendAndClear(tabId) {
        logger.info("Sending collected clicks for tab [" + tabId + "].");
        forEachAndDoClear(tabId, doSend);
    }

    function clear(tabId) {
        logger.info("Deleting collected clicks for tab [" + tabId + "].");
        forEachAndDoClear(tabId, function() {});
    }

    function isTabVisible(requestData) {
        return visibleTabs.indexOf(requestData.getTabId()) != -1;
    }

    /** Sends click data to the Engine
     * @param {Object} details Raw click data*/
    this.send = function(details) {
        var requestData = dataBuilder.createRequestData(details);
        if(requestData) {
            if(isTabVisible(requestData)) {
                doSend(requestData);
            } else {
                requests.push(requestData);
                logger.info("Collecting click [" + requestData.getUrl() + "] " +
                            "for hidden tab [" + requestData.getTabId() + "]");
            }
        }
    };

    this.onMessage = function(topic, tabId) {
        switch(topic) {
            case MESSAGING_DATA.TAB_VISIBLE:
            case MESSAGING_DATA.TAB_HIDDEN:
                visibleTabs.push(tabId);
                sendAndClear(tabId);
                break;

            case MESSAGING_DATA.TAB_RELOADED:
                clear(tabId);
                break;
        }
    };
}

/** Creates Chrome specific Web Worker
 * @constructor */
function ChromeWorkerBuilder() {
    WorkerBuilderInterface.call(this);

    this.build = function(engineCode) {
        var blob = new Blob([engineCode], {type: "application/javascript"});
        var obj = window.webkitURL.createObjectURL(blob);
        return new Worker(obj);
    }
}

dcm.addComponent("client/runner", ["client/all"], new EngineRunnerComponent(dcm, new ChromeWorkerBuilder()));
dcm.addComponent("client/runner/errors", ["client/runner"], new RunnerErrorsListenerComponent(dcm));
dcm.addComponent("client/runner/downloader", ["client/runner"], new EngineCachingComponent(dcm));

/** Writes data to the user's profile directory on disk
 * @extends StorageInterface
 * @constructor */
function FileStorage(fileName, storageType) {
    StorageInterface.call(this);

    var fileSystem;

    function errorHandler(e) {
        var msg = 'Unknown Error';

        if (e !== undefined && e.code !== undefined) {
            switch (e.code) {
                case FileError.QUOTA_EXCEEDED_ERR:
                    msg = 'QUOTA_EXCEEDED_ERR';
                    break;
                case FileError.NOT_FOUND_ERR:
                    msg = 'NOT_FOUND_ERR';
                    break;
                case FileError.SECURITY_ERR:
                    msg = 'SECURITY_ERR';
                    break;
                case FileError.INVALID_MODIFICATION_ERR:
                    msg = 'INVALID_MODIFICATION_ERR';
                    break;
                case FileError.INVALID_STATE_ERR:
                    msg = 'INVALID_STATE_ERR';
                    break;
            }
        }
    }

    function initAndExecute(onSuccess, onFailure) {
        if (fileSystem) {
            onSuccess();
        } else {
            var onInitFs = function(fs) {
                fileSystem = fs;
                onSuccess();
            };

            var requestFileSystem = window.requestFileSystem || window.webkitRequestFileSystem;
            requestFileSystem(storageType, 5 * 1024 * 1024, onInitFs, onFailure);
        }
    }

    function getFailureCallback(callback, callbackValue) {
        return function(error) {
            errorHandler(error);

            if (callback) {
                callback(callbackValue !== undefined ? callbackValue : false);
            }
        }
    }

    function doRead(onSuccess, onFailure) {
        fileSystem.root.getFile(fileName, {}, function(fileEntry) {

            // Get a File object representing the file,
            // then use FileReader to read its contents.
            fileEntry.file(function(file) {
                var reader = new FileReader();

                reader.onloadend = function(e) {
                    onSuccess(this.result);
                };

                reader.readAsText(file);
            }, onFailure);

        }, onFailure);
    }

    function doWrite(onSuccess, onFailure, value, overwrite) {
        fileSystem.root.getFile(fileName, {create : true}, function(fileEntry) {

            // Create a FileWriter object for FileEntry
            fileEntry.createWriter(function(fileWriter) {

                fileWriter.onwriteend = function(e) {
                    onSuccess(true);
                };

                fileWriter.onerror = function(e) {
                    onFailure();
                };

                // Create a new Blob and write it to file
                var blob = new Blob([value], {type : 'text/plain'});
                if (!overwrite) {
                    fileWriter.seek(fileWriter.length);
                }
                fileWriter.write(blob);

            }, onFailure);

        }, onFailure);
    }

    function doTruncate(onSuccess, onFailure) {
        fileSystem.root.getFile(fileName, {create : false}, function(fileEntry) {
            // Create a FileWriter object for FileEntry
            fileEntry.createWriter(function(fileWriter) {

                fileWriter.onwriteend = function(e) {
                    onSuccess(true);
                };

                fileWriter.onerror = function(e) {
                    onFailure();
                };
                fileWriter.truncate(0);

            }, onFailure);
        }, onFailure);
    }

    function doExist(onSuccess, onFailure) {
        fileSystem.root.getFile(fileName, {create : false}, function(fileEntry) {
            onSuccess(true);
        }, onFailure);
    }

    function doRemove(onSuccess, onFailure) {
        fileSystem.root.getFile(fileName, {create : false}, function(fileEntry) {
            fileEntry.remove(function() {
                onSuccess(true);
            }, onFailure);

        }, onFailure);
    }

    this.read = function(callback) {
        var onFailure = getFailureCallback(callback, null);
        var onSuccess = function() {
            doRead(callback, onFailure);
        };

        initAndExecute(onSuccess, onFailure);
    };

    this.write = function(callback, value) {
        var onFailure = getFailureCallback(callback);

        var writeCallback = function() {
            doWrite(callback, onFailure, value, true);
        };

        var that = this;
        var onSuccessInit = function() {
            that.exist(function(fileExists) {
                if (fileExists) {
                    doTruncate(writeCallback, onFailure);
                } else {
                    writeCallback();
                }
            });
        };
        initAndExecute(onSuccessInit, onFailure);
    };

    this.append = function(callback, value) {
        var onFailure = getFailureCallback(callback);
        var onSuccess = function() {
            doWrite(callback, onFailure, value, false);
        };

        initAndExecute(onSuccess, onFailure);
    };

    this.exist = function(callback) {
        var onFailure = getFailureCallback(callback);
        var onSuccess = function() {
            doExist(callback, onFailure);
        };

        initAndExecute(onSuccess, onFailure);
    };

    this.remove = function(callback) {
        var onFailure = getFailureCallback(callback);
        var onSuccess = function() {
            doRemove(callback ? callback : function(){}, onFailure);
        };

        initAndExecute(onSuccess, onFailure);
    };
}

/** Manages access to add-on settings
 * @param {localStorage}theStorage
 * @param {String} namePrefix. The prefix that will be added to names
 * @constructor */
function SettingsStorage(theStorage, namePrefix) {
    SettingsInterface.call(this);

    var prefixes = {
        "boolean" : "B",
        "number" : "N",
        "string" : "S"
    };

    function encodeValue(value) {
        var result = null;

        var prefix = prefixes[typeof value];
        if (prefix) {
            result = prefix + value;
        } else {
            throw "Unrecognized type of value (" + value + ")";
        }

        return result;
    }

    function decodeValue(theValue) {
        var result;

        if (theValue !== undefined && theValue !== null) {
            var prefix = theValue.charAt(0);
            var value = theValue.substr(1);

            switch (prefix) {
                case prefixes.boolean:
                    result = (value === "true");
                    break;

                case prefixes.number:
                    result = parseInt(value);
                    break;

                case prefixes.string:
                    result = value;
                    break;

                default:
                    throw "Unrecognized type of value (" + theValue + ")";
            }
        }

        return result;
    }

    function encodeKeyName(key) {
        return namePrefix + key;
    }

    function decodeKeyName(key) {
        if (key.indexOf(namePrefix) == 0) {
            return key.substr(namePrefix.length);
        }

        return null;
    }

    this.set = function(key, value) {
        var keyName = encodeKeyName(key);
        theStorage.setItem(keyName, encodeValue(value));
    };

    this.get = function(key) {
        var keyName = encodeKeyName(key);
        return decodeValue(theStorage.getItem(keyName));
    };

    this.has = function(key) {
        return this.get(key) != null;
    };

    this.remove = function(key) {
        var keyName = encodeKeyName(key);
        theStorage.removeItem(keyName);
    };

    this.removeAll = function() {
        for (var key in theStorage) {
            var newKeyName = decodeKeyName(key);
            if (newKeyName != null) {
                this.remove(newKeyName);
            }
        }
    };

    this.getAll = function() {
        var settings = {};
        for (var key in theStorage) {
            var newKeyName = decodeKeyName(key);
            if (newKeyName != null) {
                try {
                    settings[newKeyName] = this.get(newKeyName);
                } catch(e) {}
            }
        }
        return settings;
    }
}


/** Component responsible for managing user's filr and preferences
 * @param {DCMContext} context
 * @extends ComponentInterface
 * @constructor */
function StorageComponent(context) {
    ComponentInterface.call(this);

    this.create = function(config) {
        context.storageFactory = new StorageFactory();
    };

    this.cleanup = function() {
        var settings = context.storageFactory.getSettings();
        settings.removeAll();
    }
}

dcm.addComponent("client/storage", [], new StorageComponent(dcm));

/** Factory manages creating of Storage objects (e.g. file, preferences)
 * @extends StorageFactoryInterface
 * @constructor */
function StorageFactory() {
    StorageFactoryInterface.call(this);

    this.getStorage = function(name) {
        return new FileStorage(name, window.PERSISTENT);
    };

    this.getSettings = function() {
        return new SettingsStorage(localStorage, "dcm.");
    }
}

/** Chrome implementation of {NetworkFactoryInterface}
 * @param {DCMContext} context
 * @extends NetworkFactoryInterface
 * @constructor
 */
function NetworkFactory(context) {
    NetworkFactoryInterface.call(this);

    this.createNetworkUtils = function() {
        return new NetworkUtils(context);
    }
}


/** Manages SystemComponent lifecycle
 * @param {DCMContext} context
 * @constructor */
function SystemComponentRegistration(context) {
    ComponentInterface.call(this);
    var sysComp = null;

    this.create = function() {
        sysComp = new SystemComponent(context, new NetworkFactory(context));
    };
}

dcm.addComponent("client/system", ["client/communication", "client/logging"],
    new SystemComponentRegistration(dcm));


/** Manages communication between an extension and content scrips
 * @param {DCMContext} context
 * @param {chrome} theChrome
 * @param {String} portName
 * @constructor
 */
function ContentScriptCommunicator(context, theChrome, portName) {
    var messenger = context.communication.factory.createSender();

    function onAddListener(port) {
        if(port.name == portName && port.sender != undefined && port.sender.tab != undefined &&
           port.sender.tab.id != undefined) {
            assignPortListeners(port);
        }
    }

    function assignPortListeners(port) {
        var onMessageListener = createOnMessageListener(port);
        var onDisconnectListener = createDisconnectListener(onMessageListener);

        port.onMessage.addListener(onMessageListener);
        port.onDisconnect.addListener(onDisconnectListener);
    }

    function createOnMessageListener(port) {
        return function(message) {
            var sendData = {
                portName: port.name,
                tabId   : port.sender.tab.id,
                data    : message.data
            };

            if(message.id == 1) {

                messenger.send(MESSAGING_DATA.PORT_LISTENER_SUBJECT,
                    MESSAGING_DATA.TAB_VISIBILITY_CHANGED, sendData);
            } else if(message.id == 2) {
                messenger.send(MESSAGING_DATA.PORT_LISTENER_SUBJECT,
                    MESSAGING_DATA.CONTEXT_MENU_SHOWN, sendData);
            }
        }
    }

    function createDisconnectListener(messageListener) {
        return function listener(port) {
            port.onDisconnect.removeListener(listener);
            port.onMessage.removeListener(messageListener);

            var data = {
                portName: port.name,
                tabId   : port.sender.tab.id
            };
            messenger.send(MESSAGING_DATA.PORT_LISTENER_SUBJECT, MESSAGING_DATA.PORT_DISCONNECTED,
                data);
        }
    }


    this.start = function() {
        theChrome.extension.onConnect.addListener(onAddListener);
    };

    this.stop = function() {
        theChrome.extension.onConnect.removeListener(onAddListener);
    };
}

/** Manages lifecycle of the tabs. Detects creating/destroying pre-rendered pages
 * @param {DCMContext} context
 * @constructor */
function TabsComponent(context) {
    ComponentInterface.call(this);

    var portListener;
    var tabsDiagnostic;

    this.create = function(config) {
        context.tabs = {};
        context.tabs.controller = new TabsController(context, chrome);
        context.tabs.controller.start();

        portListener = new ContentScriptCommunicator(context, chrome, "page-state-tracker");
        portListener.start();

        tabsDiagnostic = new TabsDiagnostic(context, chrome);
        tabsDiagnostic.start();
    };

    this.destroy = function() {
        tabsDiagnostic.stop();

        context.tabs.controller.stop();

        portListener.stop();
    }
}

dcm.addComponent("client/tabs", ["client/communication", "client/logging"], new TabsComponent(dcm));


/** Controls tabs related functionality (e.g. pre-rendering, parent tab)
 * @param {DCMContext} context
 * @param {Object} theChrome
 * @constructor */
function TabsController(context, theChrome) {
    context.communication.factory.extendReciever(this, MESSAGING_DATA.PORT_LISTENER_SUBJECT);

    var tabs = {};
    var referringIds = {};
    var lastTabClick = {};

    var messenger = context.communication.factory.createSender();
    var logger = context.loggerFactory.getLogger("client.tabs.TabsController");

    function onTabReplaced(details) {
        logger.info("Tab [" + details.tabId + "] was replaced with tab [" + details.replacedTabId + "]");
        tabs[details.tabId] = details.replacedTabId;
    }

    function onCreatedNavigationTarget(details) {
        logger.info("onCreatedNavigationTarget. " +
                    "tabId [" + details.tabId + "]; " +
                    "sourceTabId [" + details.sourceTabId + "]; " +
                    "url [" + details.url + "]");

        referringIds[details.tabId] = { sourceTabId: details.sourceTabId, url: details.url };
    }

    function onCreated(tab) {
        logger.info("Tab was created. " +
                    "tabId [" + tab.id + "]; " +
                    "sourceTabId [" + tab.openerTabId + "]; " +
                    "url [" + tab.url + "]");

        if(tab.id != undefined && tab.url != undefined) {
            referringIds[tab.id] = { sourceTabId: tab.openerTabId, url: tab.url };
        }
    }

    function onTabVisibilityChanged(msgData) {
        var topic = null;

        switch(msgData.data) {
            case "prerender":
                topic = MESSAGING_DATA.TAB_PRERENDER;
                break;

            case "visible":
                topic = MESSAGING_DATA.TAB_VISIBLE;
                break;

            case "hidden":
                topic = MESSAGING_DATA.TAB_HIDDEN;
                break;

        }

        if (topic != null) {
            logger.info("Tab [" + msgData.tabId + "] state changed to " + msgData.data);
            messenger.send(MESSAGING_DATA.TAB_STATE_CHANGE_SUBJECT, topic, msgData.tabId);
        }
    }

    function onContextMenuShown(msgData) {
        logger.info("onContextMenuShown: " + JSON.stringify(msgData));
        lastTabClick[msgData.data] = msgData.tabId;
    }

    function onPortDisconnected(msgData) {
        logger.info("Tab [" + msgData.tabId + "] removed.");
        messenger.send(MESSAGING_DATA.TAB_STATE_CHANGE_SUBJECT, MESSAGING_DATA.TAB_RELOADED,
            msgData.tabId);
    }

    this.onMessage = function(topic, msgData) {
        switch(topic) {
            case MESSAGING_DATA.TAB_VISIBILITY_CHANGED:
                onTabVisibilityChanged(msgData);
                break;

            case MESSAGING_DATA.CONTEXT_MENU_SHOWN:
                onContextMenuShown(msgData);
                break;

            case MESSAGING_DATA.PORT_DISCONNECTED:
                onPortDisconnected(msgData);
                break;
        }
    };

    this.start = function() {
        theChrome.webNavigation.onTabReplaced.addListener(onTabReplaced);
        theChrome.webNavigation.onCreatedNavigationTarget.addListener(onCreatedNavigationTarget);
        theChrome.tabs.onCreated.addListener(onCreated);
    };

    this.stop = function() {
        theChrome.tabs.onCreated.removeListener(onCreated);
        theChrome.webNavigation.onCreatedNavigationTarget.removeListener(onCreatedNavigationTarget);
        theChrome.webNavigation.onTabReplaced.removeListener(onTabReplaced);
    };

    /** Replace new prerendered tab id with the old one
     * @param {Number} tabId to be replaced
     * @return {Number} The new tab id */
    this.getUpdatedTabId = function(tabId) {
        var newTabId = tabs[tabId];

        if(newTabId == undefined) {
            newTabId = tabId;
        }

        return newTabId;
    };

    this.getOpenerTabId = function(url, tabId) {
        var openerTabId;
        var obj = referringIds[tabId];
        if(obj != undefined && obj.url == url) {
            if(obj.sourceTabId == undefined) {
                openerTabId = lastTabClick[url];
            } else {
                openerTabId = obj.sourceTabId;
            }
        }

        logger.info("getOpenerTabId; " +
                    "url = " + url + "; " +
                    "tabId = " + tabId + "; " +
                    "openerTabId = " + openerTabId);

        return openerTabId;
    }
}

/** Sends diagnostic when tab is opened/closed
 * @param {DCMContext} context
 * @param theChrome
 * @constructor
 */
function TabsDiagnostic(context, theChrome) {

    var sender = context.communication.factory.createSender();

    function onTabCreated(tab) {
        sender.send(MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT, MESSAGING_DATA.TAB_CREATED,
            "Tab '" + tab.id + "' was opened.");
    }

    function onTabRemoved(tabId) {
        sender.send(MESSAGING_DATA.CLIENT_DIAGNOSTIC_SUBJECT, MESSAGING_DATA.TAB_CLOSED,
            "Tab '" + tabId + "' was closed.");
    }

    this.start = function() {
        theChrome.tabs.onCreated.addListener(onTabCreated);
        theChrome.tabs.onRemoved.addListener(onTabRemoved);
    };

    this.stop = function() {
        theChrome.tabs.onRemoved.removeListener(onTabRemoved);
        theChrome.tabs.onCreated.removeListener(onTabCreated);
    }
}



        })(window)
    