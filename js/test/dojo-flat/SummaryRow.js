define([
	'dojo/_base/declare',
	'dojo/_base/lang',
	'dojo/dom-construct'
], function (declare, lang, domConstruct) {
	return declare(null, {
		// summary:
		//		A mixin for dgrid components which renders
		//		a row with summary information (e.g. totals).

		// Show the footer area, which will hold the summary row
		showFooter: true,

		buildRendering: function () {
			this.inherited(arguments);

			var areaNode = this.summaryAreaNode =
				domConstruct.create('div', {
					className: 'summary-row',
					role: 'row',
					style: { overflow: 'hidden' }
				}, this.footerNode);

			// Keep horizontal alignment in sync
			this.on('scroll', lang.hitch(this, function () {
				areaNode.scrollLeft = this.getScrollPosition().x;
			}));

			// Process any initially-passed summary data
			if (this.summary) {
				this._setSummary(this.summary);
			}
		},

		_updateColumns: function () {
			this.inherited(arguments);
			if (this.summary) {
				// Re-render summary row for existing data,
				// based on new structure
				this._setSummary(this.summary);
			}
		},

		_renderSummaryCell: function (item, cell, column) {
			// summary:
			//		Simple method which outputs data for each
			//		requested column into a text node in the
			//		passed cell element.  Honors columns'
			//		get, formatter, and renderCell functions.
			//		renderCell is called with an extra flag,
			//		so custom implementations can react to it.

			var value = item[column.field] || (document.createTextNode(''));
			cell.appendChild(value);
		},

		_setSummary: function (data) {
			// summary:
			//		Given an object whose keys map to column IDs,
			//		updates the cells in the footer row with the
			//		provided data.

			var tableNode = this.summaryTableNode;

			this.summary = data;

			// Remove any previously-rendered summary row
			if (tableNode) {
				domConstruct.destroy(tableNode);
			}

			// Render row, calling _renderSummaryCell for each cell
			tableNode = this.summaryTableNode =
				this.createRowCells('td',
					lang.hitch(this, '_renderSummaryCell', data));
			this.summaryAreaNode.appendChild(tableNode);

			// Force resize processing,
			// in case summary row's height changed
			if (this._started) {
				this.resize();
			}
		}
	});
});
