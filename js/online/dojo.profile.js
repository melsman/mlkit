// Only initial IDE dependencies belong in this layer. Other Dojo.sml features
// retain their module files and are loaded by the AMD loader on demand.
var profile = {
  action: 'release',
  releaseName: '',
  optimize: false,
  layerOptimize: false, // Minified by our pinned Terser after the Dojo build.
  cssOptimize: 'comments',
  copyTests: false,
  mini: true,
  packages: [
    {name: 'dojo', location: 'dojo'},
    {name: 'dijit', location: 'dijit'},
    {name: 'dojox', location: 'dojox'}
  ],
  layers: {
    'dojo/dojo': {
      boot: true,
      customBase: true,
      include: [
        'dojo/domReady',
        // Selector plugins choose these dynamically, outside static tracing.
        'dojo/selector/acme',
        'dojo/selector/lite',
        'dojo/store/Memory',
        'dojo/store/Observable',
        'dojo/fx/Toggler',
        'dijit/tree/ObjectStoreModel',
        'dijit/Tree',
        'dijit/layout/TabContainer',
        'dijit/layout/BorderContainer',
        'dijit/layout/LayoutContainer',
        'dijit/layout/ContentPane',
        'dijit/MenuBar',
        'dijit/PopupMenuBarItem',
        'dijit/DropDownMenu',
        'dijit/MenuItem',
        'dijit/MenuBarItem'
      ]
    }
  }
};
