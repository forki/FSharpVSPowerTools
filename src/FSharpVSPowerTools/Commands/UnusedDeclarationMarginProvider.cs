﻿using Microsoft.VisualStudio.Shell;
using Microsoft.VisualStudio.Text.Editor;
using Microsoft.VisualStudio.Text.Tagging;
using Microsoft.VisualStudio.Utilities;
using System;
using System.ComponentModel.Composition;
using FSharpVSPowerTools.SyntaxColoring.UnusedSymbols;

namespace FSharpVSPowerTools
{
    [Export(typeof(IWpfTextViewMarginProvider))]
    [Name(Constants.fsharpUnusedDeclarationMargin)]
    [ContentType("F#")]
    [Order(After = PredefinedMarginNames.VerticalScrollBar)]
    [MarginContainer(PredefinedMarginNames.VerticalScrollBarContainer)]
    [TextViewRole(PredefinedTextViewRoles.Editable)]
    [TextViewRole(PredefinedTextViewRoles.PrimaryDocument)]
    public class UnusedDeclarationMarginProvider : IWpfTextViewMarginProvider
    {
        private readonly IViewTagAggregatorFactoryService _viewTagAggregatorFactoryService;
        private readonly IServiceProvider _serviceProvider;
        
        [ImportingConstructor]
        public UnusedDeclarationMarginProvider(
            [Import(typeof(SVsServiceProvider))] IServiceProvider serviceProvider,
            IViewTagAggregatorFactoryService viewTagAggregatorFactoryService)
        {
            _serviceProvider = serviceProvider;
            _viewTagAggregatorFactoryService = viewTagAggregatorFactoryService;
        }

        public IWpfTextViewMargin CreateMargin(IWpfTextViewHost wpfTextViewHost, IWpfTextViewMargin marginContainer)
        {
            var generalOptions = Setting.getGeneralOptions(_serviceProvider);
            if (generalOptions == null || !(generalOptions.UnusedReferencesEnabled || generalOptions.UnusedOpensEnabled)) return null;

 	        var textView = wpfTextViewHost.TextView;
            var tagAggregator = _viewTagAggregatorFactoryService.CreateTagAggregator<UnusedDeclarationTag>(textView);
            return new UnusedDeclarationMargin(textView, marginContainer, tagAggregator);
        }
    }
}
