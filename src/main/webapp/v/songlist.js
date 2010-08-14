
Ext.setup({
  onReady: function() {
    var tbar = new Ext.Toolbar({
      dock:'top',
      title:'Song List'
    });
    var data = new Ext.data.JsonStore({
      proxy: {
        type: 'scripttag',
        url: 'http://divaacrank.appspot.com/s/songlist',
        reader: {
          type: 'json',
          idProperty: 'key'
        }
      },
      sorters: [
        {field: 'name', direction: 'ASC'},
        {field: 'key', direction: 'ASC'}
      ],
      getGroupString: function(record) {
        return record.get('key').split('_')[1] || "";
      },
      fields: ['key', 'name'],
      autoLoad: true
    });
    var list = new Ext.List({
      height: '100%',
      disclosure: {
        scope: data,
        handler: function(record, btn, index) {
          location.href = Ext.urlAppend("song.html", Ext.urlEncode({key: record.get('key')}));
        }
      },
      store: data,
      tpl: '<tpl for="."><div class="song"><p><strong>{name}</strong></p></div></tpl>',
      itemSelector: 'div.song',
      singleSelect: true,
      grouped: true,
      indexBar: false
    });

    var panel = new Ext.Panel({
      fullscreen: true,
      styleHtmlContent: false,
      dockedItems: tbar,
      items:[list]
    });
  }
});
