Ext.setup({
  onReady: function() {
    var params = Ext.urlDecode(location.search.substring(1));
    var songKey = params['key'];
    var data = new Ext.data.JsonStore({
      proxy: {
        type: 'scripttag',
        url: 'http://divaacrank.appspot.com/s/song/' + songKey,
        reader: {
          type: 'json',
          root: 'records',
          idProperty: 'rank',
          messageProperty: 'songName'
        }
      },
      sorters: [
        {field: 'rank', direction: 'ASC'}
      ],
      fields: ['rank', 'name',
               {name: 'level', type: 'array'},
               'score', 'date'],
      autoLoad: true,
      listeners: {
        datachanged: function(store) {
          if (store.proxy.reader.rawData) {
            var tbar = Ext.getCmp('tbar');
            tbar.setTitle(store.proxy.reader.rawData['songName'] || 'failed');
          }
        }
      }
    });
    var list = new Ext.List({
      height: '100%',
      disclosure: {
        scope: data,
        handler: function(record, btn, index) {
            alert(record);
        }
      },
      store: data,
      tpl: '<tpl for="."><div class="rank"><p><strong>{name}</strong></p></div></tpl>',
      itemSelector: 'div.rank',
      singleSelect: true,
      grouped: false,
      indexBar: false
    });
    var tbar = new Ext.Toolbar({
      id: 'tbar',
      dock:'top',
      cls: 'x-toolbar-' + songKey.split('_')[1],
      title: "loading Ranking"
    });
    var panel = new Ext.Panel({
      fullscreen: true,
      styleHtmlContent: false,
      dockedItems: tbar,
      items:[list]
    });
  }
});
