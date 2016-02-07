object dmEditorManager: TdmEditorManager
  OldCreateOrder = False
  Height = 580
  Width = 675
  object aclActions: TActionList
    Images = imlMain
    OnExecute = aclActionsExecute
    Left = 216
    Top = 88
    object actSettings: TAction
      Category = 'Settings'
      Caption = 'Settings'
      Hint = 'Editor settings'
      ImageIndex = 7
      ShortCut = 24659
      OnExecute = actSettingsExecute
    end
    object actSearch: TAction
      Category = 'Find'
      Caption = '&Find...'
      Hint = 'Find'
      ImageIndex = 9
      ShortCut = 16454
      OnExecute = actSearchExecute
    end
    object actFindNextWord: TAction
      Category = 'Find'
      Caption = 'Find &next word'
      Hint = 'Find next word occurence'
      ImageIndex = 21
      ShortCut = 49192
      OnExecute = actFindNextWordExecute
    end
    object actFindPrevWord: TAction
      Category = 'Find'
      Caption = 'Find &previous word'
      Hint = 'Find previous word occurence'
      ImageIndex = 22
      ShortCut = 49190
      OnExecute = actFindPrevWordExecute
    end
    object actSearchReplace: TAction
      Category = 'Find'
      Caption = '&Replace...'
      Hint = 'Replace'
      ImageIndex = 10
      ShortCut = 16466
      OnExecute = actSearchReplaceExecute
    end
    object actSaveAs: TAction
      Category = 'File'
      Caption = 'Save as...'
      Hint = 'Save file as'
      ImageIndex = 55
      ShortCut = 24659
      OnExecute = actSaveAsExecute
    end
    object actOpen: TAction
      Category = 'File'
      Caption = 'Open'
      Hint = 'Open file'
      ImageIndex = 3
      ShortCut = 16463
      OnExecute = actOpenExecute
    end
    object actExportToHTML: TAction
      Category = 'Export'
      Caption = 'Save as HTML file'
      Hint = 'Save as HTML file'
      OnExecute = actExportToHTMLExecute
    end
    object actExportToWiki: TAction
      Category = 'Export'
      Caption = 'Save as Wiki file'
      Enabled = False
      Hint = 'Save as Wiki file'
      OnExecute = actExportToWikiExecute
    end
    object actExportToRTF: TAction
      Category = 'Export'
      Caption = 'Save as RTF file'
      Hint = 'Save as RTF file'
      OnExecute = actExportToRTFExecute
    end
    object actCopytHTMLToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy as HTML object'
      Hint = 'Copy to clipboard as HTML object'
      OnExecute = actCopytHTMLToClipboardExecute
    end
    object actCopyWikiToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy as Wiki object'
      Hint = 'Copy to clipboard as Wiki object'
      OnExecute = actCopyWikiToClipboardExecute
    end
    object actCopyRTFToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy as RTF object'
      Hint = 'Copy to clipboard as RTF object'
      OnExecute = actCopyRTFToClipboardExecute
    end
    object actCopyRTFTextToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy as RTF text'
      Hint = 'Copy to clipboard as RTF text'
      OnExecute = actCopyRTFTextToClipboardExecute
    end
    object actCopyWikiTextToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy as Wiki text'
      Hint = 'Copy to clipboard as TeX text'
      OnExecute = actCopyWikiTextToClipboardExecute
    end
    object actCopyHTMLTextToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy as HTML text'
      Hint = 'Copy to clipboard as HTML text'
      OnExecute = actCopyHTMLTextToClipboardExecute
    end
    object actPageSetup: TAction
      Category = 'Print'
      Caption = 'Page setup'
      Enabled = False
      Hint = 'Page setup'
      ImageIndex = 35
      Visible = False
      OnExecute = actPageSetupExecute
    end
    object actPrintPreview: TAction
      Category = 'Print'
      Caption = 'Print preview'
      Enabled = False
      Hint = 'Print preview'
      ImageIndex = 35
      Visible = False
      OnExecute = actPrintPreviewExecute
    end
    object actPrint: TAction
      Category = 'Print'
      Caption = 'Print'
      Enabled = False
      Hint = 'Print'
      ImageIndex = 34
      ShortCut = 16464
      Visible = False
      OnExecute = actPrintExecute
    end
    object actCopyToClipboard: TAction
      Category = 'Edit'
      Caption = 'Copy to clipboard'
      Hint = 'Copy to clipboard'
      ImageIndex = 1
      ShortCut = 16451
      OnExecute = actCopyToClipboardExecute
    end
    object actShowSpecialCharacters: TAction
      Category = 'Settings'
      AutoCheck = True
      Caption = 'Show special characters'
      Hint = 'Show special characters (like tabs and spaces).'
      ImageIndex = 84
      ShortCut = 24643
      OnExecute = actShowSpecialCharactersExecute
    end
    object actUpperCaseSelection: TAction
      Category = 'Selection'
      Caption = 'Uppercase'
      Hint = 'Upper case selection'
      ImageIndex = 20
      ShortCut = 16469
      OnExecute = actUpperCaseSelectionExecute
    end
    object actLowerCaseSelection: TAction
      Category = 'Selection'
      Caption = 'Lowercase'
      Hint = 'Lower case selection'
      ImageIndex = 25
      ShortCut = 16460
      OnExecute = actLowerCaseSelectionExecute
    end
    object actToggleComment: TAction
      Category = 'Selection'
      Caption = 'Toggle comment'
      Hint = 'Toggle comment for current line or each selected line.'
      ImageIndex = 17
      ShortCut = 16575
      OnExecute = actToggleCommentExecute
    end
    object actAlignSelection: TAction
      Category = 'Selection'
      Caption = 'Align'
      Hint = 'Align selected lines'
      ImageIndex = 29
      ShortCut = 49242
      OnExecute = actAlignSelectionExecute
    end
    object actSortSelection: TAction
      Category = 'Selection'
      Caption = 'Sort'
      Hint = 'Sort selected lines'
      ImageIndex = 16
      ShortCut = 49235
      OnExecute = actSortSelectionExecute
    end
    object actFindNext: TAction
      Category = 'Find'
      Caption = 'Find next'
      Hint = 'Find next'
      ImageIndex = 21
      ShortCut = 114
      OnExecute = actFindNextExecute
    end
    object actFindPrevious: TAction
      Category = 'Find'
      Caption = 'Find previous'
      Hint = 'Find previous'
      ImageIndex = 22
      ShortCut = 16498
      OnExecute = actFindPreviousExecute
    end
    object actQuoteLines: TAction
      Category = 'Selection'
      Caption = 'Quote lines'
      Hint = 'Quote each line in the selection.'
      ImageIndex = 82
      ShortCut = 16465
      OnExecute = actQuoteLinesExecute
    end
    object actQuoteLinesAndDelimit: TAction
      Category = 'Selection'
      Caption = 'Quote lines and delimit'
      Hint = 'Quote each line in the selection and delimit with comma'
      ImageIndex = 110
      ShortCut = 24657
      OnExecute = actQuoteLinesAndDelimitExecute
    end
    object actFormat: TAction
      Category = 'Commands'
      Caption = 'Format text'
      Hint = 'Formats text depending on the current highlighter.'
      ImageIndex = 74
      ShortCut = 49222
      OnExecute = actFormatExecute
    end
    object actIncFontSize: TAction
      Category = 'Settings'
      Caption = 'Increase font size'
      Hint = 'Increase editor font size'
      ImageIndex = 78
      SecondaryShortCuts.Strings = (
        'CTRL+'#39'+'#39)
      ShortCut = 16491
      OnExecute = actIncFontSizeExecute
    end
    object actDecFontSize: TAction
      Category = 'Settings'
      Caption = 'Decrease font size'
      Hint = 'Decrease editor font size'
      ImageIndex = 77
      ShortCut = 16493
      OnExecute = actDecFontSizeExecute
    end
    object actShowCodeShaper: TAction
      Category = 'ToolViews'
      Caption = 'Shape code'
      Hint = 'Apply custom code formatting to selection'
      ImageIndex = 79
      ShortCut = 49219
      OnExecute = actShowCodeShaperExecute
    end
    object actPascalStringOfSelection: TAction
      Category = 'Selection'
      Caption = 'Pascal string'
      Hint = 'Convert selection to pascal string declaration'
      ImageIndex = 114
      ShortCut = 57424
      OnExecute = actPascalStringOfSelectionExecute
    end
    object actSave: TAction
      Category = 'File'
      Caption = 'Save'
      Hint = 'Save'
      ImageIndex = 4
      ShortCut = 16467
      OnExecute = actSaveExecute
    end
    object actFoldLevel0: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 0'
      GroupIndex = 2
      Hint = 'Sets the fold level to 0'
      ImageIndex = 59
      OnExecute = actFoldLevel0Execute
    end
    object actFoldLevel1: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 1'
      GroupIndex = 2
      Hint = 'Sets the fold level to 1'
      ImageIndex = 60
      OnExecute = actFoldLevel1Execute
    end
    object actFoldLevel2: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 2'
      GroupIndex = 2
      Hint = 'Sets the fold level to 2'
      ImageIndex = 61
      OnExecute = actFoldLevel2Execute
    end
    object actFoldLevel3: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 3'
      GroupIndex = 2
      Hint = 'Sets the fold level to 3'
      ImageIndex = 62
      OnExecute = actFoldLevel3Execute
    end
    object actFoldLevel4: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 4'
      GroupIndex = 2
      Hint = 'Sets the fold level to 4'
      ImageIndex = 63
      OnExecute = actFoldLevel4Execute
    end
    object actFoldLevel5: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 5'
      GroupIndex = 2
      Hint = 'Sets the fold level to 5'
      ImageIndex = 64
      OnExecute = actFoldLevel5Execute
    end
    object actFoldLevel6: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 6'
      GroupIndex = 2
      Hint = 'Sets the fold level to 6'
      ImageIndex = 65
      OnExecute = actFoldLevel6Execute
    end
    object actFoldLevel7: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 7'
      GroupIndex = 2
      Hint = 'Sets the fold level to 7'
      ImageIndex = 66
      OnExecute = actFoldLevel7Execute
    end
    object actFoldLevel8: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 8'
      GroupIndex = 2
      Hint = 'Sets the fold level to 8'
      ImageIndex = 67
      OnExecute = actFoldLevel8Execute
    end
    object actFoldLevel9: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level 9'
      GroupIndex = 2
      Hint = 'Sets the fold level to 9'
      ImageIndex = 68
      OnExecute = actFoldLevel9Execute
    end
    object actFoldLevel10: TAction
      Category = 'Fold'
      AutoCheck = True
      Caption = 'Fold level max'
      Checked = True
      GroupIndex = 2
      Hint = 'Sets the fold level to 10'
      ImageIndex = 69
      OnExecute = actFoldLevel10Execute
    end
    object actToggleFoldLevel: TAction
      Category = 'Fold'
      Caption = 'Toggle fold level'
      Hint = 'Toggle folding level.'
      ImageIndex = 69
      ShortCut = 118
      OnExecute = actToggleFoldLevelExecute
    end
    object actToggleHighlighter: TAction
      Category = 'Highlighter'
      Caption = 'Toggle highlighter'
      Hint = 'Toggle highlighter.'
      ImageIndex = 75
      ShortCut = 115
      OnExecute = actToggleHighlighterExecute
    end
    object actInspect: TAction
      Category = 'Debug'
      Caption = 'Inspect'
      Hint = 'Show inspector'
      ImageIndex = 52
      ShortCut = 16496
      OnExecute = actInspectExecute
    end
    object actShowCodeFilter: TAction
      Category = 'ToolViews'
      Caption = 'Filter code'
      Hint = 'Filter code'
      ImageIndex = 70
      ShortCut = 16455
      OnExecute = actShowCodeFilterExecute
    end
    object actInsertColorValue: TAction
      Category = 'Insert'
      Caption = 'Insert color value'
      Hint = 'Insert numeric color value'
      ImageIndex = 88
      ShortCut = 8304
      OnExecute = actInsertColorValueExecute
    end
    object actReload: TAction
      Category = 'File'
      Caption = 'Reload'
      Hint = 'Reload content.'
      ImageIndex = 14
      ShortCut = 116
      OnExecute = actReloadExecute
    end
    object actNew: TAction
      Category = 'File'
      Caption = 'New'
      Hint = 'Create a new editor view.'
      ImageIndex = 39
      ShortCut = 16462
      OnExecute = actNewExecute
    end
    object actShowPreview: TAction
      Category = 'ToolViews'
      Caption = 'Show preview'
      Hint = 'Show text preview.'
      ImageIndex = 71
      ShortCut = 49232
      OnExecute = actShowPreviewExecute
    end
    object actAutoFormatXML: TAction
      Category = 'Settings'
      Caption = 'Autoformat XML'
      ImageIndex = 50
      ShortCut = 49222
      OnExecute = actAutoFormatXMLExecute
    end
    object actHelp: TAction
      Category = 'Help'
      Caption = '&Help'
      Hint = 'Show help dialog.'
      ImageIndex = 54
      OnExecute = actHelpExecute
    end
    object actOpenFileAtCursor: TAction
      Category = 'File'
      Caption = 'Open file at cursor'
      Hint = 'Open file at cursor'
      ImageIndex = 80
      ShortCut = 16397
      OnExecute = actOpenFileAtCursorExecute
    end
    object actDequoteLines: TAction
      Category = 'Selection'
      Caption = 'Dequote lines'
      Hint = 'Dequote each line in the selection.'
      ImageIndex = 86
      ShortCut = 24644
      OnExecute = actDequoteLinesExecute
    end
    object actCopy: TAction
      Category = 'Edit'
      Caption = '&Copy'
      Hint = 'Copy selection'
      ImageIndex = 1
      ShortCut = 16451
      OnExecute = actCopyExecute
    end
    object actShowCharacterMap: TAction
      Category = 'ToolViews'
      Caption = 'Show character map'
      Hint = 'Show character map with Ansi and Unicode characters.'
      ImageIndex = 43
      OnExecute = actShowCharacterMapExecute
    end
    object actQuoteSelection: TAction
      Category = 'Selection'
      Caption = 'Quote'
      Hint = 'Quote selection'
      ImageIndex = 83
      OnExecute = actQuoteSelectionExecute
    end
    object actDequoteSelection: TAction
      Category = 'Selection'
      Caption = 'Dequote'
      Hint = 'Dequote selection'
      ImageIndex = 85
      ShortCut = 16452
      OnExecute = actDequoteSelectionExecute
    end
    object actAutoGuessHighlighter: TAction
      Category = 'Commands'
      Caption = 'Auto guess highlighter'
      Hint = 'Auto guess highlighter'
      ImageIndex = 87
      ShortCut = 16456
      OnExecute = actAutoGuessHighlighterExecute
    end
    object actSmartSelect: TAction
      Category = 'Select'
      Caption = 'Smartselect'
      Hint = 'Make a smart selection'
      ImageIndex = 89
      ShortCut = 112
      OnExecute = actSmartSelectExecute
    end
    object actClose: TAction
      Category = 'File'
      Caption = 'Close active editor view'
      Hint = 'Close the active editor instance.'
      ImageIndex = 31
      ShortCut = 16499
      OnExecute = actCloseExecute
    end
    object actStripFirstChar: TAction
      Category = 'Selection'
      Caption = 'Strip first character'
      Hint = 'Strip first character from every line in the selection.'
      ImageIndex = 95
      ShortCut = 57414
      OnExecute = actStripFirstCharExecute
    end
    object actStripLastChar: TAction
      Category = 'Selection'
      Caption = 'Strip last character'
      Hint = 'Strip last character from every line in the selection.'
      ImageIndex = 94
      ShortCut = 57420
      OnExecute = actStripLastCharExecute
    end
    object actRedo: TAction
      Category = 'Edit'
      Caption = 'Redo'
      Hint = 'Redo'
      ImageIndex = 37
      ShortCut = 24666
      OnExecute = actRedoExecute
    end
    object actCloseOthers: TAction
      Category = 'File'
      Caption = 'Close all other editors'
      Hint = 'Close all other editors'
      ShortCut = 24691
      OnExecute = actCloseOthersExecute
    end
    object actMonitorChanges: TAction
      Category = 'Settings'
      Caption = 'Monitor changes'
      Hint = 'Monitor for external file changes.'
      ImageIndex = 90
      ShortCut = 24653
      OnExecute = actMonitorChangesExecute
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About'
      Hint = 'Shows the about dialog.'
      ImageIndex = 73
      OnExecute = actAboutExecute
    end
    object actShowActions: TAction
      Category = 'Debug'
      Caption = 'Show editor actions'
      Hint = 'Show the list of all available actions.'
      OnExecute = actShowActionsExecute
    end
    object actCopyFullPath: TAction
      Category = 'Edit'
      Caption = 'Copy full path'
      Hint = 'Copy full filepath to the clipboard.'
      OnExecute = actCopyFullPathExecute
    end
    object actCopyFileName: TAction
      Category = 'Edit'
      Caption = 'Copy filename'
      Hint = 'Copy the filename to the clipboard.'
      OnExecute = actCopyFileNameExecute
    end
    object actCopyFilePath: TAction
      Category = 'Edit'
      Caption = 'Copy file path'
      Hint = 'Copy the file path to the clipboard.'
      OnExecute = actCopyFilePathExecute
    end
    object actEncodeBase64: TAction
      Category = 'Selection'
      Caption = 'Encode base64'
      Hint = 'Encode selected text to base64.'
      OnExecute = actEncodeBase64Execute
    end
    object actDecodeBase64: TAction
      Category = 'Selection'
      Caption = 'Decode base64'
      Hint = 'Decode selected text from base64.'
      OnExecute = actDecodeBase64Execute
    end
    object actShowViews: TAction
      Category = 'Dialogs'
      Caption = 'Show views'
      Hint = 'Show list of all editor views.'
      ImageIndex = 91
      ShortCut = 49238
      OnExecute = actShowViewsExecute
    end
    object actSyncEdit: TAction
      Category = 'Selection'
      Caption = 'Sync edit'
      Hint = 'Synchronized edit'
      ImageIndex = 5
      ShortCut = 24650
      OnExecute = actSyncEditExecute
    end
    object actClear: TAction
      Category = 'Select'
      Caption = 'Clear'
      Hint = 'Clear all text in the editor view.'
      ImageIndex = 72
      OnExecute = actClearExecute
    end
    object actCreateDesktopLink: TAction
      Category = 'Commands'
      Caption = 'Create desktop link'
      Hint = 'Create a desktop shortcut to the active file.'
      ImageIndex = 92
      ShortCut = 49228
      OnExecute = actCreateDesktopLinkExecute
    end
    object actExit: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Exit application'
      ImageIndex = 19
      OnExecute = actExitExecute
    end
    object actStripMarkup: TAction
      Category = 'Selection'
      Caption = 'Strip markup'
      Hint = 'Strip markup tags from XML/HTML.'
      ImageIndex = 93
      ShortCut = 57427
      OnExecute = actStripMarkupExecute
    end
    object actPaste: TAction
      Category = 'Edit'
      Caption = 'Paste'
      Hint = 'Paste'
      ImageIndex = 2
      ShortCut = 16470
      OnExecute = actPasteExecute
    end
    object actCut: TAction
      Category = 'Edit'
      Caption = 'Cut'
      Hint = 'Cut'
      ImageIndex = 0
      ShortCut = 16472
      OnExecute = actCutExecute
    end
    object actUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      Hint = 'Undo'
      ImageIndex = 38
      ShortCut = 16474
      OnExecute = actUndoExecute
    end
    object actSelectAll: TAction
      Category = 'Select'
      Caption = 'Select &All'
      Hint = 'Select all.'
      ImageIndex = 23
      ShortCut = 16449
      OnExecute = actSelectAllExecute
    end
    object actDelete: TAction
      Category = 'Edit'
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 31
      ShortCut = 46
    end
    object actShowTest: TAction
      Category = 'ToolViews'
      AutoCheck = True
      Caption = 'Show testform'
      ShortCut = 49220
      OnExecute = actShowTestExecute
    end
    object actToggleBlockCommentSelection: TAction
      Category = 'Selection'
      Caption = 'Toggle block comment selection'
      Hint = 
        'Comment/uncomment the selection with the highlighter'#39's block com' +
        'ment tags.'
      ImageIndex = 17
      ShortCut = 24767
      OnExecute = actToggleBlockCommentSelectionExecute
    end
    object actShowStructureViewer: TAction
      Category = 'ToolViews'
      Caption = 'XML treeview'
      ShortCut = 49240
      OnExecute = actShowStructureViewerExecute
    end
    object actSelectionInfo: TAction
      Caption = 'SelectionInfo'
      ShortCut = 24651
      OnExecute = actSelectionInfoExecute
    end
    object actStayOnTop: TAction
      Category = 'Settings'
      AutoCheck = True
      Caption = 'Stay on top'
      Hint = 
        'Lets the application window stay on top of all other opened wind' +
        'ows.'
      ImageIndex = 96
      OnExecute = actStayOnTopExecute
    end
    object actToggleMaximized: TAction
      Category = 'Settings'
      AutoCheck = True
      Caption = 'Toggle maximized'
      Hint = 'Toggles between maximized and restored mode.'
      ImageIndex = 97
      ShortCut = 122
      OnExecute = actToggleMaximizedExecute
    end
    object actSingleInstance: TAction
      Category = 'Settings'
      AutoCheck = True
      Caption = 'Single instance'
      Hint = 'Allow only one active instance of the application.'
      ImageIndex = 98
      OnExecute = actSingleInstanceExecute
    end
    object actSelectionMenu: TAction
      Category = 'Selection'
      Caption = 'Selection'
      ImageIndex = 41
    end
    object actSelectionModeMenu: TAction
      Category = 'SelectionMode'
      Caption = 'Selectionmode'
    end
    object actHighlighter: TAction
      Category = 'Highlighter'
      Caption = 'Highlighter'
    end
    object actLineBreakStyleMenu: TAction
      Category = 'Linebreakstyle'
      Caption = 'Linebreakstyle'
    end
    object actEncodingMenu: TAction
      Category = 'Encoding'
      Caption = 'Encoding'
    end
    object actExportMenu: TAction
      Category = 'Export'
      Caption = 'Export'
      ImageIndex = 80
    end
    object actClipboardMenu: TAction
      Category = 'Clipboard'
      Caption = 'Clipboard'
      ImageIndex = 100
    end
    object actInsertGUID: TAction
      Category = 'Insert'
      Caption = 'Insert GUID'
      Hint = 'Inserts a new GUID value.'
      ImageIndex = 99
      ShortCut = 24647
      OnExecute = actInsertGUIDExecute
    end
    object actInsertMenu: TAction
      Category = 'Insert'
      Caption = 'Insert'
      ImageIndex = 102
    end
    object actFindAllOccurences: TAction
      Category = 'Find'
      Caption = 'Find all occurences'
      Hint = 
        'Finds all occurences of the selected text or the current word at' +
        ' the caret position.'
      ImageIndex = 27
      ShortCut = 24646
      OnExecute = actFindAllOccurencesExecute
    end
    object actSearchMenu: TAction
      Category = 'Find'
      Caption = 'Search'
      ImageIndex = 9
    end
    object actIndent: TAction
      Category = 'Selection'
      Caption = 'Indent'
      Hint = 'Indent selected lines.'
      ImageIndex = 30
      ShortCut = 24649
      OnExecute = actIndentExecute
    end
    object actUnindent: TAction
      Category = 'Selection'
      Caption = 'Unindent'
      Hint = 'Unindent selected lines.'
      ImageIndex = 12
      ShortCut = 24661
      OnExecute = actUnindentExecute
    end
    object actSelectMenu: TAction
      Category = 'Select'
      Caption = 'Select'
      ImageIndex = 101
    end
    object actFileMenu: TAction
      Category = 'File'
      Caption = 'File'
      ImageIndex = 103
    end
    object actShowHTMLViewer: TAction
      Category = 'ToolViews'
      Caption = 'Show HTML viewer'
      OnExecute = actShowHTMLViewerExecute
    end
    object actShowHexEditor: TAction
      Category = 'ToolViews'
      Caption = 'Show Hex editor'
      OnExecute = actShowHexEditorExecute
    end
    object actShowMiniMap: TAction
      Category = 'ToolViews'
      Caption = 'Show minimap'
      Hint = 'Show minimap.'
      ShortCut = 49229
      OnExecute = actShowMiniMapExecute
    end
    object actConvertTabsToSpaces: TAction
      Category = 'Selection'
      Caption = 'Convert TABs to spaces in selection'
      Hint = 
        'Convert TAB character to the equivalent amount of spaces in the ' +
        'selection.'
      ImageIndex = 111
      OnExecute = actConvertTabsToSpacesExecute
    end
    object actExecuteScriptOnSelection: TAction
      Category = 'Selection'
      Caption = 'Execute script on selection'
      Hint = 'Execute script function on the selected lines.'
    end
    object actShowScriptEditor: TAction
      Category = 'ToolViews'
      Caption = 'Show script editor'
      OnExecute = actShowScriptEditorExecute
    end
    object actShowFilterTest: TAction
      Caption = 'Show filter test dialog'
      ShortCut = 57460
      OnExecute = actShowFilterTestExecute
    end
    object actHighlighterMenu: TAction
      Category = 'Highlighter'
      Caption = 'Highlighter'
      ImageIndex = 75
    end
    object actFoldMenu: TAction
      Category = 'Fold'
      Caption = 'Fold'
      ImageIndex = 69
    end
    object actSettingsMenu: TAction
      Category = 'Settings'
      Caption = 'Settings'
      ImageIndex = 7
    end
    object actEncodeURL: TAction
      Category = 'Selection'
      Caption = 'Encode URL'
      Hint = 'Convert the selected text to an encoded URL string.'
      OnExecute = actEncodeURLExecute
    end
    object actDecodeURL: TAction
      Category = 'Selection'
      Caption = 'Decode URL'
      Hint = 'Decode the URL from the selected text.'
      OnExecute = actDecodeURLExecute
    end
    object actSelectionEncodeMenu: TAction
      Category = 'Selection'
      Caption = 'Encode'
      ImageIndex = 106
    end
    object actSelectionDecodeMenu: TAction
      Category = 'Selection'
      Caption = 'Decode'
      ImageIndex = 107
    end
    object actStripComments: TAction
      Category = 'Selection'
      Caption = 'Strip comments'
      Hint = 'Strip all comments from the selected code.'
      ImageIndex = 109
      OnExecute = actStripCommentsExecute
    end
    object actMergeBlankLines: TAction
      Category = 'Selection'
      Caption = 'Merge blank lines'
      Hint = 'Merge blank lines to one in the selected lines.'
      ImageIndex = 108
      OnExecute = actMergeBlankLinesExecute
    end
    object actCompressSpace: TAction
      Category = 'Selection'
      Caption = 'Compress spaces'
      Hint = 'Reduce consecutive spaces to one.'
      ImageIndex = 56
      OnExecute = actCompressSpaceExecute
    end
    object actCompressWhitespace: TAction
      Category = 'Selection'
      Caption = 'Compress whitespace'
      Hint = 
        'Replace any number of consecutive whitespace (including newlines' +
        ')'#13#10' with a single whitespace.'
      ImageIndex = 76
      OnExecute = actCompressWhitespaceExecute
    end
    object actSaveAll: TAction
      Category = 'File'
      Caption = 'Save all'
      Hint = 'Save all changes.'
      ImageIndex = 113
      ShortCut = 24659
      OnExecute = actSaveAllExecute
    end
    object actRecordMacro: TAction
      Category = 'Commands'
      Caption = 'Record macro'
      ImageIndex = 116
      ShortCut = 24658
      OnExecute = actRecordMacroExecute
    end
    object actPlaybackMacro: TAction
      Category = 'Commands'
      Caption = 'Playback macro'
      ImageIndex = 118
      ShortCut = 24656
      OnExecute = actPlaybackMacroExecute
    end
    object actEncodeXML: TAction
      Category = 'Selection'
      Caption = 'Encode XML'
      OnExecute = actEncodeXMLExecute
    end
    object actDecodeXML: TAction
      Category = 'Selection'
      Caption = 'Decode XML'
      OnExecute = actDecodeXMLExecute
    end
  end
  object ppmExport: TPopupMenu
    Images = imlMain
    Left = 40
    Top = 248
    object MenuItem1: TMenuItem
    end
  end
  object ppmHighLighters: TPopupMenu
    Images = imlMain
    Left = 40
    Top = 200
    object MenuItem7: TMenuItem
    end
  end
  object ppmClipboard: TPopupMenu
    Images = imlMain
    Left = 40
    Top = 144
    object MenuItem4: TMenuItem
    end
  end
  object dlgSave: TSaveDialog
    Options = [ofOverwritePrompt, ofHideReadOnly, ofShowHelp, ofPathMustExist, ofCreatePrompt, ofShareAware, ofEnableSizing]
    Left = 285
    Top = 32
  end
  object dlgOpen: TOpenDialog
    Left = 285
    Top = 88
  end
  object ppmFold: TPopupMenu
    Images = imlMain
    Left = 40
    Top = 88
    object MenuItem43: TMenuItem
      Action = actFoldLevel0
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF191919FF151515FF151515FF151515FF151515FF1515
        15FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF191919FF565656FFD0D0D0FFFFFFFFFFE0E0E0FF8383
        83FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF2121
        21FF212121FF212121FF393939FFF1F1F1FFFFFFFFFFC2C2C2FFF1F1F1FFFFFF
        FFFF656565FF191919FF212121FF212121FF212121FF151515FF191919FF2121
        21FF212121FF212121FFA6A6A6FFFFFFFFFFA6A6A6FF212121FF7A7A7AFFFFFF
        FFFFE0E0E0FF212121FF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF2D2D2DFFD0D0D0FFFFFFFFFF6F6F6FFF2D2D2DFF353535FFFFFF
        FFFFFFFFFFFF353535FF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3535
        35FF353535FF2D2D2DFFFFFFFFFFFFFFFFFF656565FF353535FF2D2D2DFFFFFF
        FFFFFFFFFFFF656565FF353535FF353535FF353535FF212121FF2D2D2DFF3939
        39FF393939FF2D2D2DFFFFFFFFFFFFFFFFFF656565FF353535FF2D2D2DFFFFFF
        FFFFFFFFFFFF656565FF353535FF393939FF393939FF2D2D2DFF353535FF4040
        40FF404040FF353535FFD0D0D0FFFFFFFFFF777777FF393939FF404040FFFFFF
        FFFFFFFFFFFF4D4D4DFF393939FF404040FF404040FF353535FF393939FF4040
        40FF404040FF393939FFAFAFAFFFFFFFFFFFAFAFAFFF393939FF838383FFFFFF
        FFFFE0E0E0FF393939FF404040FF404040FF404040FF393939FF404040FF4D4D
        4DFF4D4D4DFF4D4D4DFF565656FFF1F1F1FFFFFFFFFFCACACAFFF1F1F1FFFFFF
        FFFF898989FF404040FF4D4D4DFF4D4D4DFF4D4D4DFF404040FF404040FF5656
        56FF565656FF565656FF404040FF656565FFD8D8D8FFFFFFFFFFE0E0E0FF9898
        98FF404040FF565656FF565656FF565656FF565656FF404040FF4D4D4DFF5656
        56FF565656FF565656FF565656FF4D4D4DFF404040FF393939FF404040FF4D4D
        4DFF565656FF565656FF565656FF565656FF565656FF4D4D4DFF4D4D4DFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF4D4D4DFF000000004D4D
        4DFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem44: TMenuItem
      Action = actFoldLevel1
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF191919FF191919FF151515FF151515FF151515FF1515
        15FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF191919FF191919FF151515FFFFFFFFFFFFFFFFFF5656
        56FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF2121
        21FF212121FF212121FF212121FF212121FF191919FFFFFFFFFFFFFFFFFF5959
        59FF191919FF212121FF212121FF212121FF212121FF151515FF191919FF2121
        21FF212121FF212121FF212121FF212121FF212121FFFFFFFFFFFFFFFFFF5959
        59FF212121FF212121FF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF212121FFFFFFFFFFFFFFFFFF6161
        61FF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3131
        31FF313131FF313131FF313131FF313131FF2D2D2DFFFFFFFFFFFFFFFFFF6161
        61FF313131FF313131FF313131FF313131FF313131FF212121FF2D2D2DFF3939
        39FF393939FF393939FF393939FF393939FF2D2D2DFFFFFFFFFFFFFFFFFF6161
        61FF313131FF393939FF393939FF393939FF393939FF2D2D2DFF313131FF4040
        40FF404040FF404040FF393939FF313131FF313131FFFFFFFFFFFFFFFFFF6969
        69FF393939FF404040FF404040FF404040FF404040FF313131FF393939FF4040
        40FF404040FF404040FF393939FFD7D7D7FF878787FFFFFFFFFFFFFFFFFF6969
        69FF404040FF404040FF404040FF404040FF404040FF393939FF404040FF4C4C
        4CFF4C4C4CFF4C4C4CFF565656FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF6969
        69FF4C4C4CFF4C4C4CFF4C4C4CFF4C4C4CFF4C4C4CFF404040FF404040FF5656
        56FF565656FF565656FF404040FF565656FFB1B1B1FFFFFFFFFFFFFFFFFF7171
        71FF4C4C4CFF565656FF565656FF565656FF565656FF404040FF4C4C4CFF5656
        56FF565656FF565656FF565656FF565656FF4C4C4CFF393939FF393939FF4C4C
        4CFF565656FF565656FF565656FF565656FF565656FF4C4C4CFF4C4C4CFF5959
        59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
        59FF595959FF595959FF595959FF595959FF595959FF4C4C4CFF000000004C4C
        4CFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4C4C4CFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem45: TMenuItem
      Action = actFoldLevel2
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF151515FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFF151515FF191919FF191919FF151515FF151515FF2121
        21FF212121FF212121FF191919FFC2C2C2FFFFFFFFFFFFFFFFFFD0D0D0FFC2C2
        C2FFC2C2C2FFC2C2C2FF191919FF212121FF212121FF151515FF191919FF2121
        21FF212121FF212121FF212121FF212121FFA6A6A6FFFFFFFFFFF1F1F1FF4C4C
        4CFF212121FF212121FF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFFA9A9A9FFFFFFFFFFF1F1
        F1FF464646FF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3131
        31FF313131FF313131FF313131FF313131FF313131FF2D2D2DFFA9A9A9FFFFFF
        FFFFD0D0D0FF2D2D2DFF313131FF313131FF313131FF212121FF2D2D2DFF3939
        39FF393939FF393939FF393939FF393939FF393939FF313131FF393939FFE3E3
        E3FFFFFFFFFF828282FF313131FF393939FF393939FF2D2D2DFF313131FF4646
        46FF464646FF464646FF464646FF393939FF393939FF464646FF393939FFA6A6
        A6FFFFFFFFFFBDBDBDFF313131FF464646FF464646FF313131FF393939FF4646
        46FF464646FF464646FF464646FF515151FF515151FF393939FF393939FFA9A9
        A9FFFFFFFFFFBDBDBDFF393939FF464646FF464646FF393939FF464646FF4C4C
        4CFF4C4C4CFF4C4C4CFF464646FFB0B0B0FFFFFFFFFFCACACAFFD8D8D8FFFFFF
        FFFFFFFFFFFF898989FF464646FF4C4C4CFF4C4C4CFF464646FF464646FF5151
        51FF515151FF515151FF4C4C4CFF717171FFBDBDBDFFF1F1F1FFFFFFFFFFE3E3
        E3FF989898FF464646FF515151FF515151FF515151FF464646FF4C4C4CFF5151
        51FF515151FF515151FF515151FF515151FF464646FF464646FF393939FF4646
        46FF4C4C4CFF515151FF515151FF515151FF515151FF4C4C4CFF4C4C4CFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF4C4C4CFF000000004C4C
        4CFF515151FF515151FF515151FF515151FF515151FF515151FF515151FF5151
        51FF515151FF515151FF515151FF515151FF4C4C4CFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem46: TMenuItem
      Action = actFoldLevel3
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF191919FF151515FF151515FF151515FF151515FF1515
        15FF151515FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF191919FF818181FFD0D0D0FFFFFFFFFFFFFFFFFFB1B1
        B1FF343434FF191919FF191919FF191919FF191919FF151515FF151515FF2121
        21FF212121FF212121FF191919FF959595FFE1E1E1FFC2C2C2FFE1E1E1FFFFFF
        FFFFE1E1E1FF191919FF212121FF212121FF212121FF151515FF191919FF2121
        21FF212121FF212121FF212121FF404040FF212121FF212121FF212121FFE1E1
        E1FFFFFFFFFF5C5C5CFF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF343434FFE1E1
        E1FFFFFFFFFF464646FF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3434
        34FF343434FF343434FF343434FF2D2D2DFF818181FFC2C2C2FFF1F1F1FFFFFF
        FFFF9C9C9CFF2D2D2DFF343434FF343434FF343434FF212121FF2D2D2DFF4040
        40FF404040FF404040FF404040FF343434FF959595FFFFFFFFFFFFFFFFFFD0D0
        D0FF404040FF404040FF404040FF404040FF404040FF2D2D2DFF343434FF4646
        46FF464646FF464646FF464646FF404040FF404040FF343434FFADADADFFFFFF
        FFFF959595FF404040FF464646FF464646FF464646FF343434FF404040FF4646
        46FF464646FF464646FF464646FF515151FF404040FF404040FF7A7A7AFFFFFF
        FFFFCACACAFF404040FF464646FF464646FF464646FF404040FF464646FF4D4D
        4DFF4D4D4DFF4D4D4DFF464646FFA3A3A3FFE1E1E1FFCACACAFFF1F1F1FFFFFF
        FFFFA3A3A3FF464646FF4D4D4DFF4D4D4DFF4D4D4DFF464646FF464646FF5151
        51FF515151FF515151FF4D4D4DFF9C9C9CFFD8D8D8FFFFFFFFFFFFFFFFFFB1B1
        B1FF4D4D4DFF4D4D4DFF515151FF515151FF515151FF464646FF4D4D4DFF5151
        51FF515151FF515151FF515151FF4D4D4DFF464646FF404040FF404040FF4D4D
        4DFF515151FF515151FF515151FF515151FF515151FF4D4D4DFF4D4D4DFF5C5C
        5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C
        5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF4D4D4DFF000000004D4D
        4DFF515151FF515151FF515151FF515151FF515151FF515151FF515151FF5151
        51FF515151FF515151FF515151FF515151FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem47: TMenuItem
      Action = actFoldLevel4
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF1515
        15FF151515FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF191919FF191919FF191919FF151515FFFFFFFFFFFFFF
        FFFF565656FF191919FF191919FF191919FF191919FF151515FF151515FF2121
        21FF212121FF191919FF191919FF191919FF191919FF191919FFFFFFFFFFFFFF
        FFFF595959FF191919FF212121FF212121FF212121FF151515FF191919FF2121
        21FF212121FF4C4C4CFFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFFFFFFFFFFFFF
        FFFFD5D5D5FF7A7A7AFF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF616161FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF8C8C8CFF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3232
        32FF323232FF2D2D2DFFD5D5D5FFFFFFFFFF464646FF2D2D2DFFFFFFFFFFFFFF
        FFFF616161FF2D2D2DFF323232FF323232FF323232FF212121FF2D2D2DFF3939
        39FF393939FF323232FF616161FFFFFFFFFFBEBEBEFF2D2D2DFFFFFFFFFFFFFF
        FFFF616161FF323232FF393939FF393939FF393939FF2D2D2DFF323232FF4646
        46FF464646FF464646FF323232FFBEBEBEFFFFFFFFFF696969FFFFFFFFFFFFFF
        FFFF696969FF393939FF464646FF464646FF464646FF323232FF393939FF4646
        46FF464646FF464646FF464646FF565656FFF2F2F2FFAFAFAFFFFFFFFFFFFFFF
        FFFF696969FF464646FF464646FF464646FF464646FF393939FF464646FF4C4C
        4CFF4C4C4CFF4C4C4CFF4C4C4CFF464646FF969696FFFFFFFFFFFFFFFFFFFFFF
        FFFF696969FF4C4C4CFF4C4C4CFF4C4C4CFF4C4C4CFF464646FF464646FF5656
        56FF565656FF565656FF565656FF565656FF464646FFE5E5E5FFFFFFFFFFFFFF
        FFFF717171FF4C4C4CFF565656FF565656FF565656FF464646FF4C4C4CFF5656
        56FF565656FF565656FF565656FF565656FF4C4C4CFF464646FF393939FF3939
        39FF4C4C4CFF565656FF565656FF565656FF565656FF4C4C4CFF4C4C4CFF5959
        59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
        59FF595959FF595959FF595959FF595959FF595959FF4C4C4CFF000000004C4C
        4CFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4C4C4CFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem48: TMenuItem
      Action = actFoldLevel5
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF151515FF151515FF151515FF151515FF151515FF1515
        15FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF818181FFE1E1E1FFFFFFFFFFFFFFFFFFC1C1C1FF6565
        65FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF2121
        21FF212121FF191919FF959595FFE1E1E1FFC1C1C1FFD0D0D0FFFFFFFFFFFFFF
        FFFF656565FF191919FF212121FF212121FF212121FF151515FF191919FF2121
        21FF212121FF212121FF313131FF212121FF212121FF212121FF898989FFFFFF
        FFFFE1E1E1FF212121FF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF6F6F6FFFFFFF
        FFFFFFFFFFFF212121FF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3131
        31FF313131FF313131FF393939FF656565FF656565FF818181FFE1E1E1FFFFFF
        FFFFC1C1C1FF2D2D2DFF313131FF313131FF313131FF212121FF2D2D2DFF3939
        39FF393939FF313131FF656565FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1F1
        F1FF595959FF313131FF393939FF393939FF393939FF2D2D2DFF313131FF4040
        40FF404040FF404040FF313131FFFFFFFFFFE1E1E1FF959595FF777777FF4040
        40FF393939FF404040FF404040FF404040FF404040FF313131FF393939FF4040
        40FF404040FF404040FF313131FFFFFFFFFFD0D0D0FF393939FF393939FF3939
        39FF404040FF404040FF404040FF404040FF404040FF393939FF404040FF4D4D
        4DFF4D4D4DFF4D4D4DFF393939FFD8D8D8FFFFFFFFFFCACACAFFCACACAFFCACA
        CAFF898989FF404040FF4D4D4DFF4D4D4DFF4D4D4DFF404040FF404040FF5151
        51FF515151FF515151FF404040FFCACACAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFF989898FF4D4D4DFF515151FF515151FF515151FF404040FF4D4D4DFF5151
        51FF515151FF515151FF4D4D4DFF404040FF393939FF393939FF393939FF3939
        39FF4D4D4DFF515151FF515151FF515151FF515151FF4D4D4DFF4D4D4DFF5959
        59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
        59FF595959FF595959FF595959FF595959FF595959FF4D4D4DFF000000004D4D
        4DFF515151FF515151FF515151FF515151FF515151FF515151FF515151FF5151
        51FF515151FF515151FF515151FF515151FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem49: TMenuItem
      Action = actFoldLevel6
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF191919FF151515FF151515FF151515FF151515FF1515
        15FF151515FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF191919FF565656FFD0D0D0FFFFFFFFFFEFEFEFFFA1A1
        A1FF262626FF191919FF191919FF191919FF191919FF151515FF151515FF2626
        26FF262626FF191919FF595959FFFFFFFFFFFFFFFFFFC2C2C2FFE1E1E1FFFFFF
        FFFFD0D0D0FF191919FF262626FF262626FF262626FF151515FF191919FF2626
        26FF262626FF262626FFB5B5B5FFFFFFFFFF898989FF262626FF262626FFE1E1
        E1FFFFFFFFFF7A7A7AFF262626FF262626FF262626FF191919FF262626FF2D2D
        2DFF2D2D2DFF262626FFFFFFFFFFFFFFFFFF616161FF2D2D2DFF2D2D2DFFC2C2
        C2FFFFFFFFFF898989FF2D2D2DFF2D2D2DFF2D2D2DFF262626FF262626FF3131
        31FF313131FF2D2D2DFFFFFFFFFFFFFFFFFFD0D0D0FF616161FF898989FFFFFF
        FFFFFFFFFFFF727272FF313131FF313131FF313131FF262626FF2D2D2DFF3939
        39FF393939FF313131FFD0D0D0FFFFFFFFFFF1F1F1FFFFFFFFFFFFFFFFFFFFFF
        FFFFACACACFF313131FF393939FF393939FF393939FF2D2D2DFF313131FF4040
        40FF404040FF393939FFA1A1A1FFFFFFFFFFACACACFF727272FF929292FF7272
        72FF393939FF393939FF404040FF404040FF404040FF313131FF393939FF4040
        40FF404040FF404040FF565656FFF1F1F1FFFFFFFFFF929292FF393939FF3939
        39FF404040FF404040FF404040FF404040FF404040FF393939FF404040FF4D4D
        4DFF4D4D4DFF4D4D4DFF404040FF898989FFFFFFFFFFFFFFFFFFF1F1F1FFCACA
        CAFF898989FF404040FF4D4D4DFF4D4D4DFF4D4D4DFF404040FF404040FF5656
        56FF565656FF565656FF565656FF404040FF565656FFB5B5B5FFE1E1E1FFFFFF
        FFFF989898FF4D4D4DFF565656FF565656FF565656FF404040FF4D4D4DFF5656
        56FF565656FF565656FF565656FF565656FF565656FF4D4D4DFF404040FF3939
        39FF4D4D4DFF565656FF565656FF565656FF565656FF4D4D4DFF4D4D4DFF5959
        59FF595959FF595959FF595959FF595959FF595959FF595959FF595959FF5959
        59FF595959FF595959FF595959FF595959FF595959FF4D4D4DFF000000004D4D
        4DFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem50: TMenuItem
      Action = actFoldLevel7
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF151515FF151515FF151515FF151515FF191919FF1919
        19FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF262626FFEFEFEFFFFFFFFFFF727272FF191919FF1919
        19FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF2626
        26FF262626FF262626FF191919FF858585FFFFFFFFFFE1E1E1FF191919FF2626
        26FF262626FF262626FF262626FF262626FF262626FF151515FF191919FF2626
        26FF262626FF262626FF262626FF313131FFF0F0F0FFFFFFFFFF7A7A7AFF2626
        26FF262626FF262626FF262626FF262626FF262626FF191919FF262626FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF8C8C8CFFFFFFFFFFD4D4D4FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF2D2D2DFF262626FF262626FF3131
        31FF313131FF313131FF313131FF313131FF393939FFF0F0F0FFFFFFFFFF6464
        64FF313131FF313131FF313131FF313131FF313131FF262626FF2D2D2DFF3939
        39FF393939FF393939FF393939FF393939FF313131FF909090FFFFFFFFFFC9C9
        C9FF313131FF393939FF393939FF393939FF393939FF2D2D2DFF313131FF4040
        40FF404040FF404040FF404040FF404040FF393939FF404040FFF0F0F0FFFFFF
        FFFF696969FF393939FF404040FF404040FF404040FF313131FF393939FF4040
        40FF404040FF404040FF393939FF393939FF393939FF393939FF909090FFFFFF
        FFFFBDBDBDFF393939FF404040FF404040FF404040FF393939FF404040FF4D4D
        4DFF4D4D4DFF404040FFA3A3A3FFC9C9C9FFC9C9C9FFC9C9C9FFD8D8D8FFFFFF
        FFFFFFFFFFFF565656FF4D4D4DFF4D4D4DFF4D4D4DFF404040FF404040FF5656
        56FF565656FF404040FFC9C9C9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF727272FF4D4D4DFF565656FF565656FF404040FF4D4D4DFF5656
        56FF565656FF4D4D4DFF404040FF393939FF393939FF393939FF393939FF3939
        39FF393939FF4D4D4DFF565656FF565656FF565656FF4D4D4DFF4D4D4DFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF4D4D4DFF000000004D4D
        4DFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem51: TMenuItem
      Action = actFoldLevel8
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF191919FF151515FF151515FF151515FF151515FF1515
        15FF151515FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF262626FF838383FFD0D0D0FFFFFFFFFFE1E1E1FFA1A1
        A1FF262626FF191919FF191919FF191919FF191919FF151515FF151515FF2626
        26FF262626FF191919FFA1A1A1FFFFFFFFFFF1F1F1FF838383FFC2C2C2FFFFFF
        FFFFD0D0D0FF191919FF262626FF262626FF262626FF151515FF191919FF2626
        26FF262626FF262626FFFFFFFFFFFFFFFFFF6B6B6BFF262626FF313131FFFFFF
        FFFFFFFFFFFF4D4D4DFF262626FF262626FF262626FF191919FF262626FF2D2D
        2DFF2D2D2DFF2D2D2DFFE1E1E1FFFFFFFFFF8C8C8CFF2D2D2DFF616161FFFFFF
        FFFFFFFFFFFF464646FF2D2D2DFF2D2D2DFF2D2D2DFF262626FF262626FF3131
        31FF313131FF2D2D2DFF565656FFF1F1F1FFFFFFFFFFC2C2C2FFFFFFFFFFFFFF
        FFFFAAAAAAFF2D2D2DFF313131FF313131FF313131FF262626FF2D2D2DFF3939
        39FF393939FF393939FF393939FFBEBEBEFFFFFFFFFFFFFFFFFFFFFFFFFFE1E1
        E1FF393939FF313131FF393939FF393939FF393939FF2D2D2DFF313131FF4646
        46FF464646FF393939FFA1A1A1FFFFFFFFFFD0D0D0FF4D4D4DFFAAAAAAFFFFFF
        FFFFBEBEBEFF313131FF464646FF464646FF464646FF313131FF393939FF4646
        46FF464646FF393939FFCACACAFFFFFFFFFF959595FF393939FF6B6B6BFFFFFF
        FFFFFFFFFFFF313131FF464646FF464646FF464646FF393939FF464646FF4D4D
        4DFF4D4D4DFF464646FF7D7D7DFFFFFFFFFFF1F1F1FF959595FFD8D8D8FFFFFF
        FFFFBEBEBEFF464646FF4D4D4DFF4D4D4DFF4D4D4DFF464646FF464646FF5656
        56FF565656FF565656FF464646FF7D7D7DFFD8D8D8FFFFFFFFFFF1F1F1FFB1B1
        B1FF4D4D4DFF4D4D4DFF565656FF565656FF565656FF464646FF4D4D4DFF5656
        56FF565656FF565656FF565656FF4D4D4DFF464646FF393939FF464646FF4D4D
        4DFF565656FF565656FF565656FF565656FF565656FF4D4D4DFF4D4D4DFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A
        5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF4D4D4DFF000000004D4D
        4DFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
    object MenuItem52: TMenuItem
      Action = actFoldLevel9
      AutoCheck = True
      Bitmap.Data = {
        36040000424D3604000000000000360000002800000010000000100000000100
        2000000000000004000064000000640000000000000000000000000000000A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A
        0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF0A0A0AFF000000000A0A0AFF1515
        15FF151515FF151515FF151515FF151515FF151515FF151515FF151515FF1515
        15FF151515FF151515FF151515FF151515FF151515FF0A0A0AFF151515FF1919
        19FF191919FF191919FF151515FF151515FF151515FF151515FF151515FF1919
        19FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF1919
        19FF191919FF191919FF151515FFFFFFFFFFFFFFFFFFC1C1C1FF727272FF1515
        15FF191919FF191919FF191919FF191919FF191919FF151515FF151515FF2121
        21FF212121FF212121FF191919FFC1C1C1FFC1C1C1FFFFFFFFFFFFFFFFFFD1D1
        D1FF191919FF212121FF212121FF212121FF212121FF151515FF191919FF2121
        21FF212121FF212121FF212121FF212121FF212121FF404040FFD1D1D1FFFFFF
        FFFF898989FF212121FF212121FF212121FF212121FF191919FF212121FF2D2D
        2DFF2D2D2DFF2D2D2DFF2D2D2DFF6D6D6DFFB7B7B7FFB7B7B7FF898989FFFFFF
        FFFFF1F1F1FF212121FF2D2D2DFF2D2D2DFF2D2D2DFF212121FF212121FF3131
        31FF313131FF2D2D2DFF727272FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFF565656FF313131FF313131FF313131FF212121FF2D2D2DFF4040
        40FF404040FF313131FFE3E3E3FFFFFFFFFFACACACFF2D2D2DFF666666FFFFFF
        FFFFFFFFFFFF666666FF313131FF404040FF404040FF2D2D2DFF313131FF4040
        40FF404040FF313131FFFFFFFFFFFFFFFFFF6D6D6DFF404040FF313131FFFFFF
        FFFFFFFFFFFF5C5C5CFF404040FF404040FF404040FF313131FF404040FF4040
        40FF404040FF404040FFD1D1D1FFFFFFFFFF959595FF404040FF6D6D6DFFFFFF
        FFFFF1F1F1FF404040FF404040FF404040FF404040FF404040FF404040FF4D4D
        4DFF4D4D4DFF404040FF7D7D7DFFFFFFFFFFFFFFFFFFCACACAFFF1F1F1FFFFFF
        FFFF959595FF404040FF4D4D4DFF4D4D4DFF4D4D4DFF404040FF404040FF5656
        56FF565656FF565656FF404040FF7D7D7DFFD8D8D8FFFFFFFFFFE3E3E3FF9898
        98FF404040FF565656FF565656FF565656FF565656FF404040FF4D4D4DFF5656
        56FF565656FF565656FF565656FF4D4D4DFF404040FF404040FF404040FF4D4D
        4DFF565656FF565656FF565656FF565656FF565656FF4D4D4DFF4D4D4DFF5C5C
        5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C
        5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF5C5C5CFF4D4D4DFF000000004D4D
        4DFF565656FF565656FF565656FF565656FF565656FF565656FF565656FF5656
        56FF565656FF565656FF565656FF565656FF4D4D4DFF00000000}
      GroupIndex = 1
      RadioItem = True
    end
  end
  object dlgColor: TColorDialog
    CustomColors.Strings = (
      'ColorA=000000'
      'ColorB=000080'
      'ColorC=008000'
      'ColorD=008080'
      'ColorE=800000'
      'ColorF=800080'
      'ColorG=808000'
      'ColorH=808080'
      'ColorI=C0C0C0'
      'ColorJ=0000FF'
      'ColorK=00FF00'
      'ColorL=00FFFF'
      'ColorM=FF0000'
      'ColorN=FF00FF'
      'ColorO=FFFF00'
      'ColorP=FFFFFF'
      'ColorQ=C0DCC0'
      'ColorR=F0CAA6'
      'ColorS=F0FBFF'
      'ColorT=A4A0A0')
    Left = 285
    Top = 144
  end
  object ppmEncoding: TPopupMenu
    Images = imlMain
    Left = 40
    Top = 315
    object MenuItem19: TMenuItem
    end
  end
  object ppmLineBreakStyle: TPopupMenu
    Images = imlMain
    Left = 117
    Top = 88
    object MenuItem6: TMenuItem
    end
  end
  object ppmSelectionMode: TPopupMenu
    Images = imlMain
    Left = 117
    Top = 32
    object MenuItem3: TMenuItem
    end
  end
  object ppmSelection: TPopupMenu
    Images = imlMain
    Left = 117
    Top = 144
    object MenuItem5: TMenuItem
    end
  end
  object ppmEditor: TPopupMenu
    Images = imlMain
    Left = 40
    Top = 32
    object MenuItem2: TMenuItem
    end
  end
  object ppmInsert: TPopupMenu
    Images = imlMain
    Left = 121
    Top = 215
    object MenuItem8: TMenuItem
    end
  end
  object ppmSearch: TPopupMenu
    Images = imlMain
    Left = 117
    Top = 280
    object MenuItem9: TMenuItem
    end
  end
  object ppmSettings: TPopupMenu
    Images = imlMain
    Left = 200
    Top = 280
    object MenuItem10: TMenuItem
    end
  end
  object ppmSelect: TPopupMenu
    Images = imlMain
    Left = 200
    Top = 215
    object MenuItem11: TMenuItem
    end
  end
  object ppmFile: TPopupMenu
    Images = imlMain
    Left = 280
    Top = 224
    object MenuItem12: TMenuItem
    end
  end
  object ppmSelectionEncode: TPopupMenu
    Images = imlMain
    Left = 88
    Top = 464
    object MenuItem21: TMenuItem
    end
  end
  object ppmSelectionDecode: TPopupMenu
    Images = imlMain
    Left = 184
    Top = 464
    object MenuItem22: TMenuItem
    end
  end
  object imlMain: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Masked = False
    Left = 320
    Top = 272
    Bitmap = {
      494C01015E006100040010001000FFFFFFFF2000FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000008001000001002000000000000080
      0100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000061D0F88155F
      30F2176935FF155F30F2061D0F88000000000000000000000000000000000000
      0000000000000000000000030647001B36CC001B36CC00030648000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000061C0E84268C51FF62BA
      8DFF95D2B2FF62BA8DFF268C51FF071F108C0000000000000000000000000000
      00000000000000030648001B35CC5294B7FF32659AFF001D38CA000306480000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000196434F760BA8BFF5EBA
      87FF000000005EB987FF65BC8FFF176333F70000000000000000000000000000
      000000040741002740BB386F9FFF366D9DFF5D9FC0FF4376ABFF00203AC60003
      0746000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002F794AFF9CD4B6FF0000
      0000000000000000000095D2B2FF176935FF0000000000000000000000000003
      0648001B35CC4D8DB3FF66ACC8FF4680ACFF4E87B3FF68AAC8FF5388BBFF0024
      3DC1000407440000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003E7F55F790D3B1FF92D6
      B1FF0000000063BC8CFF65BC8FFF176333F700000000000000000005083F002A
      43B65DA1C0FF3D77A3FF4076A7FF64A6C5FF5F9DC2FF5C95C1FF72B4D1FF6398
      CBFF000000AB0000003C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000003020202303737
      37DF5A5A5AF4636363F7656565F90B0B0B5E05050543587563F95FAB81FF95D4
      B4FFBAE6D0FF68BB8FFF2B8F55FF071F108C0000000000030648001B35CC3166
      98FF4E8CB3FF67ABC8FF65A7C6FF4B80B3FF6FB1CEFF6CA9CDFF6AA3CEFF6B6B
      6BFFAA9999FF000000A500040742000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000029353535D77C7C
      7CEF939393EDAEAEAEFF979797FF6F6F6FFF767676FF979797FF85A18FFF5A93
      6FFE4D8E64FF3F8054FD0E221591000000000005083E002C44B168AEC9FF64A8
      C5FF5492B8FF4980AFFF5B97BFFF75B9D2FF649DC8FF79BAD5FF7C7C7CFFCEC0
      C0FF777777FF5388BBFF002134A6000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000007070769595959EF1E1E
      1E742A2A2AB93D3D3DFA444444FF4F4F4FFF4F4F4FFF444444FF3D3D3DFA2A2A
      2AB91E1E1E74595959EF070707690000000000192683083850B812455EC32157
      71D047859DE76EB3C9FB6CACCCFF649DC8FF83C7DAFF888888FFD3CACAFF8383
      83FF5EA4C6FF61A7C9FF002236A5000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000E0E0E8A797979F71717
      17670000000A383838F3BDBDBDFFCECECEFFC2C2C2FFADADADFF383838F30000
      000A17171767797979F70E0E0E8A000000000000000500000014000203280006
      094100141F77063044AB367288DB75B5CFFC919191FFD9D4D4FF8D8D8DFF66AC
      CEFF72B8D4FF002A42B400050840000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000404044E646464F0B9B9
      B9EE2C2C2CB6474747FC636363FF929292FF777777FF636363FF474747FC2C2C
      2CB6828282E1646464F00404044E000000000000000000000000000000000000
      0000000000000000001000111A6D00000069DDDCDCFF949494FF6EB4D6FF80C4
      DBFF002B44B2000F3ECC00010648000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000061515158A9191
      91FFE8E8E8FFDDDDDDFFC1C1C1FF616161DE464646C2CDCDCDF8DDDDDDFFC4C4
      C4FF919191FF1515158A00000006000000000000000000000000000000000000
      0000000000000000000000000000000000240000006788CCDDFF87CBDDFF002C
      43AF001E51CC3D70B6FF001849CC000208480000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000B1515
      1575737373EE9E9E9EFF5E5E5EE104040437000000185C5C5CDD9E9E9EFF7373
      73EE151515750000000B00000000000000000000000000000030000000BC0000
      001800000018000000BC0000003000000000000203390025389C0024379C0005
      083E00040A48002256CC5085C9FF001C4FCC0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000017000000B9000000460000
      00000000000000000046000000B9000000170000000000000000000000000000
      00000000000000040B4800255ACC00040A480000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000005B00000072000000000000
      00000000000000000000000000720000005B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000017000000B3000000430000
      00000000000000000043000000B3000000170000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000002C000000B00000
      001600000016000000B00000002C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000493022A9C38E66FFC08B64FFBE8862FFBB855FFFB9835DFFB47C5AFFB179
      56FFAE7755FFAD7454FFA96F4FFF493022A90000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000088A7BBFF464E53FF424343FF3D3F3FFF3D4548FF7B9CB1FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000C8926AFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFA9704FFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000005D6C75FFBCBCBBFFEBEAEAFFCDCCCCFFA3A19FFF3D4A53FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00070B0F145728364AA84E6A93E95676A8F55477ACF65071A7F42E4264C20E15
      2072010204290000000000000000000000000000000000000000000000000000
      0000D8A277FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFEFFFFFEFEFFFFFFFFFFB7815CFF0000000000000000B07856FFB078
      56FFB07856FF00000000DD9BD9FFDD9BD9FFDD9BD9FF00000000B175FFFFB175
      FFFFB175FFFF0000000000000000000000000000000000000000000000000000
      00000000000066757BFFA6A5A2FFA8A2A2FF9D9998FF948F8BFF414951FF0000
      0000000000000000000000000000000000000000000000000000595D62B190A3
      BFFB85AADDFF8DB4E9FF9BBBE9FFA5C0E7FFA5BFE6FFA1BDE7FF90B1E3FF6A9A
      E3FF5E84C1FE515D72CA00000004000000000000000000000000000000000000
      0000D9A377FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FEFFFFFEFEFFFEFEFEFFFFFFFFFFBA855EFF0000000000000000B07856FFB078
      56FFB07856FF00000000DD9BD9FFDD9BD9FFDD9BD9FF00000000B175FFFFB175
      FFFFB175FFFF0000000000000000000000000000000000000000000000000000
      000085A7BFFF618195FF7895A3FF388A98FF337D8CFF5E6C74FF2B4155FF7DA2
      BEFF0000000000000000000000000000000000000000777C82C69CBCE3FF9DC0
      ECFFA5B6CCEEABB2BFE6AFB2B5E0B1B1B1DEB1B1B1DEB1B1B1DEB0B0B0DEA3AD
      BDE691B3E7FF6496E1FF5B7397E600000001000000000000000038251A94966D
      4FE0DBA478FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFBD8761FF0000000000000000B07856FFB078
      56FFB07856FF00000000DD9BD9FFDD9BD9FFDD9BD9FF00000000B175FFFFB175
      FFFFB175FFFF00000000000000000000000000000000000000000000000073B9
      EAFF5FA7DEFF449DE6FF49BEF7FF45E6FDFF3FE5FDFF4FC3FBFF147ADEFF3182
      D1FF64AAE3FF0000000000000000000000004B4E5199ADC8E8FFC0D3EAFFE0E6
      EDFFEFEFEFFFE9DAD1FFE1B9A1FFDA9C75FFD9986FFFDFB095FFE8D4C9FFF0F0
      F0FFE6E9EEFFB5C9E9FF6496E2FF4D5C77C600000000000000009A7052E0C4C4
      C4E0DCA779FFDCA779FFDCA779FFDCA779FFDCA779FFDCA779FFDCA779FFDCA7
      79FFDCA779FFDCA779FFDCA779FFC08B64FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000076BDEBFF5AAC
      E7FFA6D3F3FF63AEF0FF72E1F6FF71E1F6FF70E0F6FF6FE0F6FF4AA3ECFF9CC3
      EFFF277DD6FF63A8E2FF0000000000000000A9B7C5EED7E0E9FFF0F0F0FFF4F4
      F4FFF2E4DCFFE7B79AFFE2A179FFE19D71FFDF986AFFDE9362FFE2A27AFFEFDB
      CFFFF4F4F4FFF0F0F0FFCBD5E7FF7199D4F90000000000000000A67D5BE0C4C4
      C4E0DDAD86FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B9
      92FFE8B992FFE8B992FFE8B992FFBD8E6BFD0000000000000000B2EBD0FFB2EB
      D0FFB2EBD0FF000000006BCC4EFF6BCC4EFF6BCC4EFF00000000EBB05EFFEBB0
      5EFFEBB05EFF0000000000000000000000000000000080C6F0FF66B5E9FFA5D4
      F3FFDCFAFEFF36A1EBFF72E1F6FF68E4F6FF5BE2F5FF70E0F6FF1491E8FFC0F5
      FDFFACCEF1FF2580D6FF6DAEE4FF00000000D7DADEFCEEEEEEFFF4F4F4FFF9F9
      F9FFF0D1BEFFE9B18EFFE7AB86FFE5A67CFFE4A075FFE39B6EFFE19666FFEBBB
      9EFFF9F9F9FFF4F4F4FFEEEEEEFF92AFDBFE0000000000000000A77E5BE0C4C4
      C4E0BAA498EDDCB28FFEDCA779FFDCA678FFDAA478FFD8A277FFD5A074FFD29D
      71FFCF9A70FFCE996EFFC49A77FF1D130D6B0000000000000000B2EBD0FFB2EB
      D0FFB2EBD0FF000000006BCC4EFF6BCC4EFF6BCC4EFF00000000EBB05EFFEBB0
      5EFFEBB05EFF0000000000000000000000000000000076C0ECFF8BC8EFFFECFC
      FEFF75E1F7FF2D99EAFF73E1F6FF72E1F6FF66DEF5FF71E1F6FF0786E6FF44D5
      F3FFDCFEFEFF6DAAE5FF4A99DEFF00000000AFC8DDFEDCE2E8FFF7F7F7FFFBFB
      FBFFF0C4ABFFECB898FFECB491FF19110DFF17100CFFE7A478FFE59D71FFE7A4
      7BFFFBFBFBFFF7F7F7FFD2DCECFF668FCCFF2B1C148273533CC4CC9970F8EBE5
      E2F8EAE5E2F8EAE4E1F8E9E4E1F8E9E4E1F8E9E4E1F8E9E4E0F8E9E3E0F8DCD9
      D6F0C4C4C4E092684BE000000000000000000000000000000000B2EBD0FFB2EB
      D0FFB2EBD0FF000000006BCC4EFF6BCC4EFF6BCC4EFF00000000EBB05EFFEBB0
      5EFFEBB05EFF000000000000000000000000000000006DBEECFFC9E9F9FFD4F9
      FDFF7AE3F7FF86E5F8FF5EB1EFFF66B5EFFF61B4EFFF4AA6ECFF82E4F7FF57DC
      F5FF8AEBFAFFCBE2F7FF318BD9FF00000000AEB6BCE8ADCBE5FFBDCAD6FFFBFB
      FBFFF1C9B2FFEFBFA2FFEEBA9BFF1B130EFF19120DFFEAAA84FFE8A57AFFE9AA
      84FFFBFBFBFFC4D2E7FF79A6E5FF7195C9FB75553EC4969696C4D3A680F8D3A6
      80F8D3A680F8D3A680F8D3A680F8D3A680F8D3A680F8D3A680F8D3A680F8CB99
      6FF8A9815DE0936A4DE000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000079C5EEFFDFF6FDFFC8F5
      FCFFCDF6FCFFD6F7FDFFD3F4FCFFCFF2FCFFCAF1FBFFC4F0FCFFBAF2FBFF96EA
      F8FF70E5F7FFE2F4FDFF2F89D8FF000000004444448EC5D6E6FFB7D1ECFF97AB
      BFFFD6CAC5FFEFC4A9FFEFC0A4FFEEBB9DFFEDB695FFEBB18EFFE9AB85FFEFC7
      B1FFA4BAD9FF83ACE4FF86B1F0FF4E5359A07F5E46C4969696C4D2A888F7DCB5
      95F8DCB595F8DCB595F8DCB595F8DCB595F8DCB595F8DCB595F8DCB595F8D6AA
      85F8B28E70E0926C53DE00000000000000000000000000000000696DFEFF696D
      FEFF696DFEFF0000000071AAFFFF71AAFFFF71AAFFFF0000000065D5F0FF65D5
      F0FF65D5F0FF0000000000000000000000000000000088CDF1FFD2EFFBFFDBF9
      FEFFDFF9FDFFECFBFEFFEEFCFEFFEFFCFEFFEFFCFEFFEBFBFEFFE0F9FEFFB8F1
      FBFFA8F1FBFFCBE5F8FF3692DCFF0000000000000000777777B9C0D3E6FFBBD3
      ECFFA1B9D0FF8FA0B3FF9BA1ABFFB3A8A6FFB4A5A1FFB2A19BFF9B9DA6FF819D
      C0FF94BAECFF91B8EEFF818E9FD2000000007F5F46C4969696C49F8E83DAD1AD
      91F6D3A680F8D3A57FF8D2A37FF8D0A17EF8CD9F7BF8CA9D79F8C89B78F8C08F
      66F897775BE0160F0A5E00000000000000000000000000000000696DFEFF696D
      FEFF696DFEFF0000000071AAFFFF71AAFFFF71AAFFFF0000000065D5F0FF65D5
      F0FF65D5F0FF0000000000000000000000000000000098D6F4FFB4E3F8FFE5FA
      FEFFDBF8FDFFE4FAFEFFF0FCFEFFF9FEFFFFF9FEFFFFEFFCFEFFD2F6FDFFB4F1
      FBFFEDFDFFFF69B3EAFF56A9E4FF0000000000000000000000005A5A5AA1C3D0
      DAF9BAD3E8FFBED5EEFFB6CFE9FFA5BED9FF9FB8D5FF9FBBDCFFAAC8F1FFA3C3
      EEFFA0BFE8FE666B71B20000000000000000816047C4969696C4969696C49696
      96C4969696C4969696C4969696C4969696C4969696C4969696C4969696C46F4F
      39C4000000000000000000000000000000000000000000000000696DFEFF696D
      FEFF696DFEFF0000000071AAFFFF71AAFFFF71AAFFFF0000000065D5F0FF65D5
      F0FF65D5F0FF00000000000000000000000000000000B2E1F5FFA2DBF4FFC3EB
      FAFFE2F9FDFFE0F9FDFFD5F7FDFFCFF6FDFFC9F4FCFFC7F4FCFFD6F9FDFFEBFA
      FEFF90CAF2FF41A2E4FF76BEE9FF000000000000000000000000000000000C0C
      0C3C6C7072B5B8C6D5F6BCD0E5FFB8CEE6FFB4CBE6FFB1C9E6FFAABED9F8757D
      88C40C0C0C3C000000000000000000000000826247C4826247C4826247C48262
      47C4826247C4826247C4826247C4826247C4826247C4826247C4826247C47151
      3BC4000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AFE0F6FFADDE
      F6FFB7E4F8FFC7ECFBFFD7F3FCFFE1F7FDFFE2F8FEFFD8F0FCFFB6DFF8FF69BB
      EDFF54AFE8FF75BEECFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000080644CC2896D55C4896D55C4896D
      55C4896D55C4896D55C4896D55C4896D55C4896D55C4896D55C4896D55C47052
      3FC2000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B0E2F5FFA7DCF5FF9DD9F5FF91D1F1FF82CBF0FF74C4EFFF6BBFEDFF75C3
      EEFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000110B085278604CBC826247C48261
      47C4806047C47D5E44C47D5C44C47C5B43C47A5A42C4795941C4735A46C4110B
      0852000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FFFF0000FFFF000000000000
      00007C8686FF7C8686FF7C8686FF7C8686FF7C8686FF7C8686FF7C8686FF7C86
      86FF7C8686FF7C8686FF7C8686FF748585FF0000FFFF0000FFFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003C3C3CFF151515FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000F1D1D1D8F3434
      34BF5E5E5EFF5E5E5EFF5D5D5DFF343434BF0303032F000000005D5D5DFF5D5D
      5DFF5E5E5EFF0000000000000000000000000000FFFF0000FFFF0000FFFF0000
      0000B0B0B0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FF7C8686FF0000FFFF0000FFFF0000FFFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000676767FFC9C9C9FF959595FF1414
      14FF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000F525252EF7D7D
      7DFFB4B4B4FFBBBBBBFFB3B3B3FF919191FF525252EF000000005E5E5EFFBBBB
      BBFF5D5D5DFF000000000000000000000000000000000000FFFF0000FFFF0000
      FFFFB2B2B2FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FF7C8686FF000000000000FFFF0000FFFF0000
      FFFF4B6363FF4B6363FF1B7474FF0000000000000000000000001B7474FF4B63
      63FF4B6363FF3D6868FF0000000000000000676767FFDEDEDEFF2F2F2FFF4343
      43FF131313FF0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000303032F5252
      52EF5D5D5DFF5E5E5EFF6D6D6DFFBBBBBBFF5D5D5DFF000000005E5E5EFFBBBB
      BBFF5E5E5EFF00000000000000000000000000000000000000000000FFFF0000
      FFFF0000FFFFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FF7C8686FF00000000000000000000FFFF0000
      FFFF0000FFFF9A9A9AFF9B9A9AFF1F7575FF000000001D7373FF929292FF9696
      96FF949494FF9E9B9BFF416D6DFF0000000000000000414141FF818181FF3333
      33FF3C3C3CFF131313FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005E5E5EFFBBBBBBFF5D5D5DFF000000005D5D5DFFB9B9
      B9FF5D5D5DFF0000000000000000000000000000000000000000000000000000
      FFFF0000FFFF0000FFFFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FF7C8686FF00000000000000009B9B9BFF0000
      FFFF0000FFFF0000FFFF959595FF7B7B7BFF000000007D7D7DFF8F8F8FFF8686
      86FF868686FF888888FF9C9C9CFF000000000000000000000000454545FF8383
      83FF353535FF3D3D3DFF131313FF000000000000000000000000000C11450000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005E5E5EFFBABABAFF5C5C5CFF000000005E5E5EFFBABA
      BAFF5D5D5DFF0000000000000000000000000000000000000000000000000000
      00000000FFFF0000FFFF0000FFFFC0C0C0FFC0C0C0FFC0C0C0FFC0C0C0FFC0C0
      C0FFC0C0C0FFC0C0C0FFC0C0C0FF7C8686FF00000000575F5FFFA1A1A1FF9696
      96FF0000FFFF0000FFFF0000FFFF898989FF2E6E6EFF9A9898FF868686FF8686
      86FF868686FF868686FFB4B4B4FF000000000000000000000000000000005656
      56FF848484FF353535FF3D3D3DFF131313FF0000000000090D3C005A7BB70000
      000000000006000D124800000000000000000000000000000000000000000000
      0000000000000000000F5D5D5DFFBBBBBBFF5E5E5EFF000000005E5E5EFFBABA
      BAFF5E5E5EFF00000000000000000000000000000000000000002D6E6EFF336B
      6BFF898989FF0000FFFF0000FFFF0000FFFF929292FF838383FF979797FFB5B4
      B4FFC0C0C0FFC0C0C0FFC0C0C0FF7C8686FF00000000575F5FFF9B9B9BFFA3A3
      A3FF9A9A9AFF0000FFFF0000FFFF0000FFFF2E6E6EFFA7A7A7FF8C8B8BFF9290
      90FF868686FF898989FF8D8B8BFF000000000000000000000000000000000000
      00004C4C4CFF656565FF353535FF3E3E3EFF141414FF003B4F9300AFEBFC0008
      0B3900749ECF001D286900000000000000000000000000000000000000001717
      177F484848DF5D5D5DFF6C6C6CFFBBBBBBFF5E5E5EFF000000005D5D5DFFBABA
      BAFF5E5E5EFF000000000000000000000000000000002B6F6FFF949494FFA2A2
      A2FF9E9E9EFF7A7A7AFF0000FFFF0000FFFF0000FFFFA3A3A3FF8E8E8EFF8786
      86FFC0C0C0FFC0C0C0FFC0C0C0FF7C8686FF00000000575F5FFFA3A1A1FF4276
      76FF7A7878FF909090FF0000FFFF0000FFFF0000FFFFADACACFF6A8282FF706F
      6FFF909090FF7D7B7BFF287070FF000000000000000000000000000000000000
      0000000000004C4C4CFF656565FF363636FF404040FF4B4B4BFF49CCF5FF17BD
      F2FF0BB7F2FF0002031E000000000000000000000000000000001D1D1D8F6C6C
      6CFFA2A2A2FFBBBBBBFFBABABAFFBABABAFF5E5E5EFF000000005D5D5DFFBBBB
      BBFF5D5D5DFF00000000000000000000000000000000747878FF999999FF9696
      96FF8D8D8DFF797979FF959494FF0000FFFF0000FFFF0000FFFF999999FF7474
      74FFC0C0C0FFC0C0C0FFBFBFBFFF7C8686FF00000000575F5FFFBCBBBBFF1078
      78FF0000000000000000000000000000FFFF0000FFFF0000FFFF3B6B6BFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000002031E085B74B14D4D4DFF656565FF8C8C8CFFA9A9A9FF3F97B4FF75D9
      F8FF00B1EBFC007499CC002C3B7F0000000000000000000000005E5E5EFFAEAE
      AEFFADADADFF959595FF929292FFBBBBBBFF5E5E5EFF000000005E5E5EFFBBBB
      BBFF5D5D5DFF00000000000000000000000000000000B2B2B2FF9B9999FF9C9B
      9BFF979797FF747474FF596B6BFFA5A5A5FF0000FFFF0000FFFF0000FFFF5968
      68FF528484FF528484FF528484FF4D8484FF0000000000000000A1A1A1FF7676
      76FF077B7BFF0000000000000000000000000000FFFF0000FFFF0000FFFF1675
      75FF000000000000000000000000000000000000000000000000000000000000
      000000000000000101150B627DB74A4A4AFFCACACAFFF7F7F7FFD3D3D3FF49A1
      BFFF72D9F8FF00A4DAF300080B380000000000000000000000005D5D5DFFBABA
      BAFF989898FF969696FF959595FFBBBBBBFF5D5D5DFF000000005E5E5EFFBBBB
      BBFF5E5E5EFF00000000000000000000000000000000B6B6B6FF476A6AFF8684
      84FF939191FF147878FF396969FF989898FF516565FF0000FFFF0000FFFF0000
      FFFF000000000000000000000000000000000000000000000000376A6AFFB0AE
      AEFF7B7B7BFF1C7474FF0000000000000000000000000000FFFF0000FFFF0000
      FFFF1D7575FF0000000000000000000000000000000000000000000000000000
      00000000000000000000072E3B7E4FCFF6FF4DA5C2FFF8F8F8FFFEFEFEFF58B0
      CDFF68D6F7FF0080A8D50002031F0000000000000000000000005E5E5EFFBBBB
      BBFF9B9B9BFF989898FF959595FFBBBBBBFF5D5D5DFF000000005D5D5DFFBABA
      BAFF5E5E5EFF00000000000000000000000000000000909090FF217575FF0000
      00000000000000000000396969FF808080FF0A7A7AFF000000000000FFFF0000
      FFFF0000FFFF0000000000000000000000000000000000000000000000003C6E
      6EFFACACACFFA9A9A9FF737272FF087A7AFF00000000000000000000FFFF0000
      FFFF0000FFFF787878FF137777FF000000000000000000000000000000000000
      000000010218166B86BD23C1F5FF4CCEF7FF83DDF8FF4CA4C2FF59B1CFFF66BF
      DCFF3FCAF6FF06BCF1FF00405497000000000000000000000000535353EFA3A3
      A3FFAEAEAEFF989898FF979797FFBBBBBBFF5E5E5EFF5E5E5EFF5E5E5EFFBBBB
      BBFF5E5E5EFF5E5E5EFF000000000000000000000000466565FF9B9A9AFF2276
      76FF027C7CFF0000000000000000758080FF828282FF117979FF000000000000
      FFFF0000FFFF0000FFFF00000000000000000000000000000000000000000000
      0000386A6AFF6F7676FF9E9D9DFF287070FF0000000000000000000000000000
      FFFF0000FFFF0000FFFF516161FF000000000000000000000000000000000000
      0000000000000000000000000000092F3B7E58D0F7FF6BD7F8FF7BDCF8FF49CD
      F6FF0FAFE1F60000000C000000000000000000000000000000000C0C0C5F6363
      63FF9C9C9CFFBBBBBBFFBABABAFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBB
      BBFFBBBBBBFF5D5D5DFF000000000000000000000000000000004C6A6AFFA6A6
      A6FF797979FF1C7474FF0000000000000000747C7CFF9C9C9CFF686868FF0000
      00000000FFFF0000FFFF0000FFFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000FFFF0000FFFF0000FFFF000000000000000000000000000000000000
      00000000000000000000000000001F85A5D215647DB7126079B44ECEF7FF0628
      337515A9D6F002171D5A00000000000000000000000000000000000000000808
      084F484848DF5E5E5EFF5D5D5DFF5E5E5EFF5E5E5EFF5E5E5EFF5E5E5EFF5E5E
      5EFF5E5E5EFF5D5D5DFF00000000000000000000000000000000000000004665
      65FF575F5FFF1A7474FF000000000000000000000000575F5FFF575F5FFF0000
      0000000000000000FFFF0000FFFF0000FFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FFFF0000FFFF0000FFFF0000000000000000000000000000
      000000000000000000000002031E08232B6C000000000310154B1B80A0CF0000
      000000030321041E266600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FFFF0000FFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FFFF0000FFFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000009071F27660000
      000000000000000000000000000000000000000000000000001D000000340000
      003600000036000000360000003600000036000000360000003600721DF70179
      1CFF00040050000000330000001D000000000000000000000000000000000000
      000000000000070A0B57424141C93C3B3BC33C3B3BC33C3B3BC33C3B3BC33C3B
      3BC33D3C3CC42F2F2FB800000029000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000034E5E5E5F5F8F8
      F8FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFF098831FF41A1
      5DFF198934FFC0D2C4F5000000330000000001060C60071E35B7082039BD071F
      38BC051C35B8415059D3FEF9F5FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9F9FFF9F9
      F9FFFBFBFAFFDDDDDDF60B0B0B7A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005E5E
      5ECF8F8F8FFF8F8F8FFF909090FF8F8F8FFF909090FF909090FF909090FF9090
      90FF909090FF8F8F8FFF5E5E5ECF000000000000000100000036FAFAFAFEFCFC
      FCFFFCFCFCFFFCFCFCFF209750FF1A9148FF148F42FF0E8B3AFF389F5CFF80C1
      96FF44A360FF168932FF0006015700000000051427A286A1C3FF3E6BA1FF3B68
      9FFF2D5D98FF6C7B8FFFFAF8F7FFE7E5E5FFE7E5E5FFE7E5E5FFE7E5E5FFE7E5
      E5FFEBE9E9FFF0EFEFFB0B0B0B7A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000009090
      90FFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBBBB
      BBFFBBBBBBFFBBBBBBFF8E8E8EFF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF279B59FF90CAA9FF8DC8A5FF8AC6A1FF88C59EFF68B6
      85FF82C297FF46A564FF00691AEE00040031051529A48AA6C6FF4271A6FF3E6F
      A3FF2F639DFF6D7D91FFFBF9F8FFE9E8E9FFE9E8E7FFE9E8E7FFE9E8E7FFE9E8
      E7FFEDECEBFFF0F0F0FB0B0B0B7A00000000000000000303032F484848DF5D5D
      5DFF5E5E5EFF474747DF0303032F00000000000000000303032F484848DF5C5C
      5CFF5E5E5EFF484848DF0303032F000000000000000000000000000000009090
      90FFBABABAFFBBBBBBFFBABABAFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFBABA
      BAFFBABABAFFBBBBBBFF8F8F8FFF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF2F9F61FF94CDADFF6DBA8EFF69B889FF64B685FF5FB3
      80FF65B582FF83C298FF3AA05AFF007B23FC051528A38DA8C9FF487AB1FF487A
      B1FF2F639DFF6D7D91FFEBEAEAFFEBEAEAFFEBEAEAFFEBEAEAFFEBEAEAFFEBEA
      EAFFEFEEEEFFF0F0F0FB0B0B0B7A000000000101011F525252EF919191FFBBBB
      BBFFBBBBBBFF969696FF484848DF000000000101011F535353EF909090FFBABA
      BAFFBBBBBBFF959595FF484848DF000000000000000000000000000000008F8F
      8FFFBBBBBBFFBBBBBBFFBBBBBBFFBBBBBBFFB9B9B9FFBBBBBBFFBBBBBBFFBABA
      BAFFBBBBBBFFBBBBBBFF909090FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF35A369FF96CEB0FF94CDADFF91CBAAFF90CBA8FF72BC
      90FF8AC7A1FF44A566FF058533FD00000010051528A390ACCBFF487AB1FF487A
      B1FF2F639DFF6D7D91FFEEEFF1FFEEEDEDFF0095FFFF0095FFFFEEEDEDFFEEED
      EDFFF2F1F1FFF0F0F0FB0B0B0B7A000000002424249F848484FFABABABFF9191
      91FF969696FFBBBBBBFF5D5D5DFF000000002424249F858585FFA5A5A5FF8484
      84FF8E8E8EFFBBBBBBFF5E5E5EFF000000000000000000000000000000009090
      90FFBBBBBBFFBBBBBBFFBBBBBBFFBABABAFFBBBBBBFFBABABAFFB9B9B9FFBBBB
      BBFFBABABAFFBABABAFF909090FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF3BA56DFF37A46CFF33A266FF2F9E60FF53AF7AFF91CB
      AAFF4DAB72FF179044FF0001003F00000001051528A394AFCDFF5182B2FF4779
      B2FF2F639DFF0095FFFF0095FFFFF1EFEFFF0095FFFF0095FFFFF1EFEFFF0497
      FFFF0095FFFFF1F0F0FB0B0B0B7A00000000535353EFAEAEAEFF9D9D9DFF9292
      92FF939393FFBABABAFF5E5E5EFF00000000525252EFAEAEAEFF939393FF8585
      85FF878787FFBABABAFF5D5D5DFF000000000000000000000000000000008989
      89FFA5A5A5FFA6A6A6FFB5B5B5FFBBBBBBFFB6B6B6FFA5A5A5FFA5A5A5FFA6A6
      A6FFB4B4B4FFBBBBBBFF8F8F8FFF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFAFAFAFF37A367FF58B3
      81FF269855FFF4F8F6FF0000003600000001051528A396B2D0FF5D8BB8FF5D8B
      B8FF0095FFFF18FFFFFF00C6FFFF0095FFFF18FFFFFF35FFFFFF0497FFFF04C7
      FFFF32FCFCFF0095FFFF0B0B0B7A000000005E5E5EFFBBBBBBFF8A8A8AFFA2A2
      A2FFBABABAFF919191FF535353EF000000005E5E5EFFBBBBBBFF888888FFA2A2
      A2FFBBBBBBFF919191FF535353EF00000000000000001717177F535353EF5D5D
      5DFF656565FF919191FFA6A6A6FF8B8B8BFF636363FF5D5D5DFF656565FF9191
      91FFA4A4A4FFB9B9B9FF8F8F8FFF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FF3CA66EFF2F9F
      63FFEBEFEDFFFCFCFCFF0000003600000001051528A399B4D3FF5D8BB8FF5D8B
      B8FF2F639DFF0095FFFF5BFFFFFF8CFFFFFFB6FFFFFFB6FFFFFF8CFFFFFF5BFF
      FFFF0095FFFFF2F1F1FB0B0B0B7A000000005E5E5EFF959595FF535353EF5252
      52EF5E5E5EFF3E3E3ECF0808084F000000005E5E5EFF969696FF525252EF5252
      52EF5E5E5EFF3E3E3ECF0808084F000000000C0C0C5F636363FFA2A2A2FFBBBB
      BBFF8A8A8AFF656565FF848484FF636363FFA3A3A3FFBBBBBBFF8B8B8BFF6565
      65FFA5A5A5FFBBBBBBFF909090FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF2F2F2FFEFEF
      EFFFEDEDEDFFFCFCFCFF0000003600000001051528A39BB7D7FF5D8BB8FF5D8B
      B8FF2F639DFF6D7D91FF0095FFFF00C6FFFFCCFFFFFFCCFFFFFF00C6FFFF0095
      FFFF00000000F3F2F2FB0B0B0B7A00000000484848DF797979FF343434BF0000
      000000000000000000000000000000000000484848DF787878FF343434BF0000
      000000000000000000000000000000000000484848DF9C9C9CFFA4A4A4FF9898
      98FFBBBBBBFF5D5D5DFF646464FF9C9C9CFF9D9D9DFF8E8E8EFFB9B9B9FF5E5E
      5EFFA8A8A8FFBBBBBBFF909090FF000000000000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF1F1F1FFECECECFFEAEA
      EAFFE6E6E6FFFCFCFCFF0000003600000001051528A3A0BBD7FF6694C1FF5D8B
      B8FF5787B7FF0095FFFF5BFFFFFF8CFFFFFFB6FFFFFFB6FFFFFF8CFFFFFF5BFF
      FFFF0095FFFF5B5B5BD401010134000000001616167F737373FF636363FF1717
      177F000000000000000000000000000000001717177F737373FF626262FF1717
      177F000000000000000000000000000000005E5E5EFFB3B3B3FFA1A1A1FFB1B1
      B1FFA2A2A2FF626262FF5E5E5EFFB4B4B4FF9E9E9EFFAEAEAEFFA3A3A3FF6363
      63FF8F8F8FFF8F8F8FFF5E5E5ECF000000000000000100000036FCFCFCFFF9F9
      F9FFF9F9F9FFF9F9F9FFF7F7F7FFF6F6F6FFF2F2F2FFEBEBEBFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF0000003600000001051528A3A4BEDAFF6896C2FF5D8B
      B8FF0095FFFF18FFFFFF00C6FFFF0095FFFF18FFFFFF2BF7F7FF0497FEFF03C6
      FDFF27E8E9F70095FFFF00000001000000000000000F535353EF848484FF6D6D
      6DFF484848DF1717177F0101011F000000000000000F535353EF858585FF6D6D
      6DFF484848DF1717177F0101011F000000005D5D5DFF7D7D7DFF5E5E5EFF5E5E
      5EFF5D5D5DFF0C0C0C5F5E5E5EFF7C7C7CFF5C5C5CFF5E5E5EFF5D5D5DFF0C0C
      0C5F000000000000000000000000000000000000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFFCFCFCFFF6F6
      F6FFF4F4F4FF313131910000002000000000051529A4A6C1DDFF88A6C4FF8E91
      93FF737578FF0095FFFF0095FFFF727577FF0095FFFF0293FAFF295176D80293
      FAFF008EF2F9000000000000000000000000000000000303032F525252EF7878
      78FF8B8B8BFF808080FF535353EF00000000000000000303032F535353EF7878
      78FF8B8B8BFF7D7D7DFF535353EF00000000474747DF626262FF2424249F0000
      00000000000000000000484848DF636363FF2424249F00000000000000000000
      0000000000000000000000000000000000000000000000000036F7F7F7FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFE7E7
      E7FF2F2F2F91000000200000000200000000051427A1B3CAE5FFB8CADEFF9D9E
      A1FFF0F1F3FFEFEEEEFFEAEBEBFFE6E7EAFF0095FFFF0698FFFF67839BE20715
      30C00000000000000000000000000000000000000000000000000000000F1D1D
      1D8F474747DF5C5C5CFF474747DF0000000000000000000000000000000F1D1D
      1D8F484848DF5E5E5EFF474747DF000000001717177F5C5C5CFF636363FF2C2C
      2CAF0101011F000000001717177F5E5E5EFF636363FF2C2C2CAF0101011F0000
      0000000000000000000000000000000000000000000000000033DBDBDBF0F7F7
      F7FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF2F2F
      2F910000002000000002000000000000000000030642122942C0122944C20E26
      40BF213C5BD8FCF9F8FFD3D9DFF80E2844C60D2540C10E2743C4081B31AF0104
      0A59000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002B2B2BAF686868FF7373
      73FF535353EF00000000000000002C2C2CAF686868FF737373FF535353EF0000
      000000000000000000000000000000000000000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      0020000000020000000000000000000000000000000000000000000000000000
      0000171C1D65A8A4A4F792908FEA000001160000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001D1D1D8F5E5E
      5EFF535353EF0000000000000000000000001D1D1D8F5D5D5DFF535353EF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000F131313892C2C
      2CCD393939EB2B2B2BCC17171798010101280A0A0A652A2A2ACB363636E40A0A
      0A6500000000000000000000000000000000000000000000000F131313892C2C
      2CCD393939EB2B2B2BCC17171798010101280A0A0A652A2A2ACB363636E40A0A
      0A65000000000000000000000000000000000000000000000000000000003303
      07FF330307FF0401002400000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFFFFFCFCFCFFFDFDFDFFFEFEFEFFF5F5F5FFC6C6C5FF6C6B
      6BDB0000000000000000000000000000000000000018363636E3868686FFCCCC
      CCFFE5E5E5FFCFCFCFFFA2A2A2FF474747F95C5C5CFFC9C9C9FFDCDCDCFF4545
      45FB0000000000000000000000000000000000000018363636E3868686FFCCCC
      CCFFE5E5E5FFCFCFCFFFA2A2A2FF474747F95C5C5CFFC9C9C9FFDCDCDCFF4545
      45FB000000000000000000000000000000000000000000000000000000003303
      07FF330307FFB64E06DFB84F06E00602002B0000000000000000000000000000
      0000000000010000000000000000000000000000000000000000000000000000
      000000000000BABABAFFB9B9B9FFE3E3E3FFFDFDFDFFEFEFEEFFA8A7A7FFF8F8
      F8FF6A6A69DB0000000000000000000000001E1E1EA5949494FFECECECFFEAEA
      EAFFC5C5C5FFC0C0C0FFE8E8E8FFE2E2E2FFC6C6C6FFF3F3F3FFF0F0F0FF4C4C
      4CFF000000000000000000000000000000001E1E1EA5949494FFECECECFFEAEA
      EAFFC5C5C5FFC0C0C0FFE8E8E8FFE2E2E2FFC6C6C6FFF3F3F3FFF0F0F0FF4C4C
      4CFF000000000000000000000000000000000000000000000000000000000000
      000000000000000000001B0B0157C25307E60803003000000000000000000000
      85BC00009BCB00000001000000000000000000000000FFFFFFFFFCFCFCFFFDFD
      FDFFFEFEFEFFF5F5F5FFC6C6C5FFA3A3A2FFD9D9D9FFF0F0F0FFA09F9FFFFDFD
      FDFFF8F8F8FF696867DC0000000000000000424242EADBDBDBFFEAEAEAFF6868
      68FF2D2D2DC1242424AF4D4D4DF4B6B6B6FFF5F5F5FFF2F2F2FF666666FF2D2D
      2DC100000000000000000000000000000000424242EADBDBDBFFEAEAEAFF6868
      68FF2D2D2DC1242424AF4D4D4DF4B6B6B6FFF5F5F5FFF2F2F2FF666666FF2D2D
      2DC1000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002811016A6F2F04AE00000000000000160A00
      82C7000085BC00000000000000000000000000000000FFFFFFFFFCFCFCFFFDFD
      FDFFFDFDFDFFEFEFEEFFA8A7A7FFF8F8F8FFA09F9EFFD2D2D2FFA8A8A8FFFEFE
      FEFFFDFDFDFFF8F8F8FF6B6A6ADC00000000555555FCF4F4F4FFF2F2F2FF5151
      51F70000000E0000000001010126585858F9E9E9E9FFF0F0F0FF595959FF0000
      000800000000000000000000000000000000555555FCF4F4F4FFF2F2F2FF5151
      51F70000000E0000000001010126585858F9E9E9E9FFF0F0F0FF595959FF0000
      0008000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000702002CAB4905D800000017290001930000
      000A0000000000000000000000000000000000000000FFFFFFFFFCFCFCFFFDFD
      FDFFFDFDFDFFF0F0F0FFA09F9FFFFDFDFDFFF8F8F8FF9E9D9BFFC5C5C5FFA8A8
      A8FFA09F9FFFA8A7A6FFCBCAC8FF00000000383838C8E8E8E8FFFBFBFBFFB9B9
      B9FF565656F4272727A70F0F0F693C3C3CD0ECECECFFFAFAFAFF616161FF0000
      000800000000000000000000000000000000383838C8E8E8E8FFFBFBFBFFB9B9
      B9FF565656F4272727A70F0F0F693C3C3CD0ECECECFFFAFAFAFF616161FF0000
      0008000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000001C0C01597D3204BE000000170000000A0000
      00000000000000000000000000000000000000000000FEFEFEFFFCFCFCFFFDFD
      FDFFFEFEFEFFF7F7F7FFA8A8A8FFFEFEFEFFFDFDFDFFD4DAD2FF92948FFFDBDB
      DBFFEFEFEFFFEEEEEEFFF6F6F5FF0000000012121270949494FFF3F3F3FFFAFA
      FAFFF6F6F6FFDBDBDBFFBABABAFF8E8E8EFFEAEAEAFFFDFDFDFF6A6A6AFF0000
      00080000000000000000000000000000000012121270949494FFF3F3F3FFFAFA
      FAFFF6F6F6FFDBDBDBFFBABABAFF8E8E8EFFEAEAEAFFFDFDFDFF6A6A6AFF0000
      0008000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000EB44906E3430702AF0000000A000000000000
      00000000000000000000000000000000000000000000FFFFFFFFFCFCFCFFFDFD
      FDFFFEFEFEFFFDFDFDFFEAEAEAFFA8A8A8FFA09F9FFF1B4616FF264716FFA3AD
      9FFFFDFDFDFFFDFDFDFFFEFEFEFF0000000000000001161616767A7A7AFCBABA
      BAFFD5D5D5FFEBEBEBFFF8F8F8FFFBFBFBFFFBFBFBFFFBFBFBFF707070FF0000
      00080000000000000000000000000000000000000001161616767A7A7AFCBABA
      BAFFD5D5D5FFEBEBEBFFF8F8F8FFFBFBFBFFFBFBFBFFFBFBFBFF707070FF0000
      0008000000000000000000000000000000000000000000000000000000000000
      000000000000000000005A2403A0592103A40000000A00000000000000000000
      00000000000000000000000000000000000000000000FFFFFFFFFBFBFBFFFCFC
      FCFFFDFDFDFFFEFEFEFFFCFCFCFFF7F7F7FFEFEFEFFF15541BFF0AAC73FF1755
      19FF92A98BFFFCFCFCFFFDFDFDFF000000000000000000000000020202261111
      116626262697434343C76F6F6FF8B4B4B4FF4747B2FF000087FF00007BFF0000
      79FF000083FF000047B400000000000000000000000000000000020202261111
      116626262697434343C76F6F6FF8B4B4B4FFE7EFE7FF62A563FF418743FF0001
      001F000000000000000000000000000000000000000000000000000000000000
      0000000000000000EAFA0000F6FF010000140000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFFFFFBFBFBFFFCFC
      FCFFFCFCFCFFFDFDFDFFFEFEFEFFFEFEFEFFFDFDFDFF1C601FFF09D299FF0AD7
      A0FF167C3CFF5B8753FFE9EFE9FF00000000363636AD767676FF767676FF4242
      42BF000000130000000003030329797979FE00008DFF001DCDFF102CD1FF223B
      D6FF334ADBFF000087FF0000000000000000363636AD767676FF767676FF4242
      42BF000000130000000003030329797979FE5AA05CFF1D8E4AFF1A8946FF1F53
      1DD8000000180000000000000000000000000000000000000000000000000000
      00000000000C0903E7FD0000EAFA000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFFFFFAFAFAFFFBFB
      FBFFFCFCFCFFFCFCFCFFFDFDFDFFFDFDFDFFFDFDFDFF266E25FF1EBE85FF22C8
      91FF21C48EFF369F61FF75A472FF00000000616161E2E4E4E4FFEFEFEFFFBDBD
      BDFF838383FF747474F7888888FFBFBFBFFF00008AFF102CD1FF223BD6FF1924
      9EFF000070FF0000278C0000000000000000616161E2E4E4E4FFEFEFEFFFBDBD
      BDFF838383FF747474F7888888FFBFBFBFFF60A362FF1B8A47FF059964FF1689
      49FF194313D711300DBB205A17FF12300EB40000000000000000000000000000
      00175E1409C6693512A900000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEFEFEFFF9F9F9FFFAFA
      FAFFFBFBFBFFFBFBFBFFFCFCFCFFFCFCFCFFFCFCFCFF328230FF70D0AAFF68D0
      A8FF4EB275FF549D59FFD2E2D2FF000000002C2C2C95B6B6B6FFECECECFFEDED
      EDFFEDEDEDFFECECECFFEFEFEFFFEFEFEFFF00008FFF223BD6FF1924A7FF4558
      DFFF2B33AFFF000061D700000118000000002C2C2C95B6B6B6FFECECECFFEDED
      EDFFEDEDEDFFECECECFFEFEFEFFFEFEFEFFFDEE6DFFF478A46FF178B4BFF07B5
      81FF128C4FFF194A0DFF0ADEA9FF1E5615FF0000000000000000000000000900
      00476A3717AC1B0F065700000000000000000000000000000000000000000000
      00000000000000000000330503FF330503FF00000000FEFEFEFFF7F7F7FFF9F9
      F8FFFAFAFAFFFAFAFAFFFBFBFBFFFBFBFBFFFBFBFBFF3A8E3BFF9ADEBFFF51A7
      62FF76B37AFFF1F7F2FFFFFFFFFF000000000000000D595959CCA7A7A7FFD3D3
      D3FFE0E0E0FFE5E5E5FFDDDDDDFFC8C8C8FF000099FF334ADBFF000083FF2B33
      B3FF6876E9FF3C42C1FF000074D7000001180000000D595959CCA7A7A7FFD3D3
      D3FFE0E0E0FFE5E5E5FFDDDDDDFFC8C8C8FFAEAEAEFF686E68E51E4F1BDB1490
      52FF09D09BFF0E8F56FF0BECB6FF1C5111FF0000000000000015200001830000
      0000553118973D24128000000000000000000000000000000000000000000000
      0000000000000000000D330503FF330503FF00000000FFFFFFFFF5F5F4FFF6F6
      F6FFF8F8F8FFF9F9F9FFFAFAFAFFFAFAFAFFFAFAFAFF4D9E53FF4A9D52FF0D21
      0E8B0000003B00000000000000000000000000000000000000031212125B5151
      51BF686868D8797979E95F5F5FCF2D2D2D8F0E0E45A700009CFF000050BB0000
      68D73C42C4FF8E96F2FF4E51D4FF00006FC300000000000000031212125B5151
      51BF686868D8797979E95F5F5FCF2D2D2D8F0D0D0D4E0000000B153912BA215D
      19FF12975CFF0BECB6FF0CF1BBFF1D5313FF000077B20A0095D50000000B0000
      000005030125CD8048EA0E09053F000000000000000000000000000000000000
      0000000000055F3B219F5B38209C0000000000000000FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD6E9D8FFDEEDDFFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0118000079D74E51D5FF4E51DBFF000079CC0000000000000000000000000000
      00000000000000000000000000000000000000000000000000002A7127FF0ADE
      A9FF0BECB6FF0CF1BBFF0CF1BBFF215D19FF000063A200006CA9000000000000
      000000000000110B0744BC7B4CDF644128A319100A53030201200704022C1D13
      0C59AD7247D6815434B901000011000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000118000079CC00006FC3000001180000000000000000000000000000
      0000000000000000000000000000000000000000000000000000173D16B4296F
      25FF256720FF23621DFF24631DFF133410B40000000000000000000000000000
      000000000000000000000000000930211671744F36AFB98057DDA2714CCF714E
      35AD160F0A4E0000000000000000000000000000000000000000000000000000
      000000000000000000000001021F007988ED007887EC00353C9E000202210000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001D130A653522
      1287352212873522128735221287352212873522128735221287352212873522
      1287352212871D130A6500000000000000000000000000000000000000000000
      000000000000010C1351156DA5EA147EC0FE137DC0FE1678B4F5146293DD0832
      4CA00001021C0000000000000000000000000000000000000000000000000000
      00000000000000000013006E7CE24DBFCEFD59D3E1FF2CADBEFA068695F6003D
      45A9000000140000000000000000000000000000000000000000000000000000
      00000201002323130C7D6A3B25DB7D472EF37C462EF3643A26DB20130C7D0201
      0023000000000000000000000000000000000000000000000000311F11820000
      00000000000000000000FEFEFEFFFEFEFEFFFEFDFCFFFDFCFBFFFDFBF9FFFCFA
      F8FFFDFBF9FF311F118200000000000000000000000000000000000000000000
      00000001021E298AC6FECAE7F3FFE4F4F9FFE7F5F9FFE0F2F8FFD8EDF6FF9CCE
      E7FF2086C4FE093651A500000007000000000000000000000000000000000000
      00000000000000545EC532ADBDFA5DD8E7FF24CADFFF4CD4E5FF6BD9E7FF30AE
      BFFB007280E60002032600000000000000000000000000000000000000001008
      05537C4429E6B28055FFD5B793FFDBC3A6FFDAC3A6FFD2B490FFAB7850FF6C3E
      29E60E08055300000000000000000000000000000000000000002C1C0F7B0000
      0000FEFEFEFFFEFEFEFFFEFDFCFFFDFBF9FFFCFAF8FFFCF9F6FFFBF7F3FFFAF6
      F2FFFBF8F5FF2C1C0F7B0000000000000000212121FF212121FF121212FB0808
      08EC0B2A3DAE9ECEE7FFAFE0CFFF008676FF47B99BFF97D8ECFFA4DDEDFFDBF1
      F7FFE4F3F9FF67AFD7FE062A3F93000000000000000000000000000000000000
      000000090A4317909FF66ADAE8FF0EC5DCFF01C2DAFF01C2DAFF13C6DCFF59D7
      E7FF54C6D4FE03808FF10008093F000000000000000000000000110A0553914D
      2DF4CBA77BFFD8BB9FFFC39C75FFB68A60FFB4865EFFBE9670FFD1B397FFC5A3
      75FF78472FF40E0805530000000000000000391E0299663504CC663504CCB375
      29FFB37529FFB37529FFB37529FFB37529FFB37529FFB37529FFB37529FFB375
      29FFB37529FF663504CC663504CC06030033000000000A0A0AB8656565FF2727
      27FF111619FED7ECF5FF1BA763FF32E1CBFF008676FF8ED5EAFF4263DBFF0000
      CCFF8399E7FFE2F3F8FF2488C5FE000406300000000000000000000000000000
      000000616ED45CCFDDFF44DAEDFF16D0E7FF0FCBE3FF05C4DCFF01C2DAFF01C2
      DAFF4AD3E4FF56CDDCFF028291F40001011C000000000301002289532AE5CFAA
      81FFDABCA2FFBE9164FFBA8C60FFB7895DFFB3845CFFB1835BFFB0835AFFCDAA
      8DFFC6A577FF6E3F29E5020100220000000074420FCCFFB507FFFFB609FFFFB7
      0BFFFFB80EFFFFB912FFFFBB19FFFFC028FFFFC638FFFFCB48FFFFD055FFFFD2
      5EFFFFD361FFFFD565FFD5A138F374420FCC000000000202026D131313FF6565
      65FF131313FFA9B6BDFF4ABA9CFF11A35CFF47BA9EFF8AD2E9FF0D13D2FF0077
      F5FF0000CCFFD8F0F7FFAFD7EBFF08304A9E0000000000000000000000000001
      011A0F8A9BF78BEDFBFF3AE5FCFF35E4FBFF2DDEF6FF21D7EEFF12CDE5FF02C3
      DBFF01C2DAFF54D6E6FF31B0C0FB003D44A8000000002C1D0C7EBF915CFFE0C2
      A8FFC5966AFFC29167FFE1CBB8FFFEFDFCFFFFFFFEFFEADCD0FFB4855CFFB385
      5CFFD4B599FFAE7954FF22130C7E00000000482C0D9983521ACC83521ACCD79B
      4DFFD79B4DFFD79B4DFFD79B4DFFD79B4DFFD79B4DFFD79B4DFFD79B4DFFD79B
      4DFFD79B4DFFE2AF61FFFFE18CFF83521ACC0000000000000000060606B21313
      13FFCFD7D3FF858A88FF649DB1FF81CCE6FF82CDE6FF83CEE7FF4064DBFF0D14
      D2FF466CDDFF9BD8EBFFD3EAF4FF115A87D40000000000000000000000000011
      155F2AA2B1F58AEFFDFF5DEAFDFF5FEBFDFF50E9FDFF3AE6FDFF28DBF3FF16D0
      E7FF0EC6DCFF47D2E4FF65D4E2FF007684E9000000008A5B26DBDBBC9CFFD5AD
      89FFC7986AFFC39567FFC19365FFEDDFD3FFFAF7F4FFBB8B61FFB98A61FFB88A
      60FFC59D76FFD2B893FF6A3B26DB00000000000000000704003337231389FEFE
      FBFFFBFBF9FFFAFAF7FFF9F9F5FFFAF6F2FFF7F7F1FFF8F1ECFFF8F0EAFFF7EE
      E7FFF9F2EDFFDEBA9CFFE1BF75F3905F23CC000000000000000000000000031C
      2B7A929292FFECEEEEFF858A88FF5B98AFFF79C8E4FF7AC9E4FF7BC9E5FF7DCA
      E5FF80CBE6FF8AD0E8FFDEF0F7FF1577B3F50000000000000000000000070019
      73D7113FA9FC84EAFBFF69ECFDFF84EFFDFF6BECFDFF50E9FDFF42E5FBFF63E3
      F3FF75DDEBFF47BFCDFE178F9EF3007584EA00000000B17430F6E3C7AFFFD0A2
      74FFC59969FFC49768FFC49667FFEEE0D4FFFBF7F4FFBF9064FFBE8F63FFBE8F
      62FFBE9267FFDFC6AAFF88492FF60000000006030033663504CC4A2F1A9FFDFD
      FAFFC5985CFFB27428FFD9BE99FFF9F3EFFFF8F1ECFFF8F0EAFFF7EEE7FFF4E8
      DEFFF6EDE6FF56371EAB956528CC090602330000000000040733116AA1E8459B
      CCFDD6EBF5FF858A88FF5E8CBCFF00578FFF5795ADFF73C4E2FF75C5E3FF8583
      75FF87481EFF868A80FFE3F3F8FF127DC0FE000000000012155F007886EB2599
      ADF62648BBFF1B36B8FF5FD8F6FF61EAFDFF6AEBFDFF7BEEFDFF88EAF8FF41B6
      C5FA038695F7003940A3000809400000000200000000B77B35F6E4C9B0FFD0A3
      78FFCC9D6FFFC79A6AFFC59869FF00000000FFFFFEFFC39667FFC19466FFC294
      66FFC3986BFFDFC5ABFF8B4B2FF6000000006D3C0ACCD49310F3CA9060FFFDFD
      F9FFBC8032FFFFC434FFBC8032FFDCC19BFFF8F0EAFFF7EEE7FFF4E8DEFFF1E2
      D5FFF5EBE3FF34221286090602330000000001090F492185C4FEB7DBEDFFDCEE
      F6FFDEEFF6FFAED9EBFF00578FFF5E8CBCFF00578FFF5292ACFF6EC0E0FF8B56
      31FFCF9F70FF87481EFFDCEEF6FF1576B3F5000000000011135A008393F64DC8
      D8FF66DCECFF316BC8FF0B19ABFF60B1D7FF70DCEAFF3DB1BFF7078696F60036
      3D9F0001011B0000000000000000000000000000000095682FDBE0BC9FFFDBB3
      93FFCFA073FFCD9E70FFCB9C6FFFDDBFA3FFDDBFA2FFC59969FFC59969FFC498
      69FFD1AB85FFD8BA97FF753E26DB00000000764510CCFFC73BFFD4993BFFC78B
      3DFFC78B3DFFEAAD29FFFFD565FFC78B3DFFDFC49EFFF4E8DEFFF1E2D5FFEFDD
      CFFFF4E9E0FF22160C6D0000000000000000106498E1C2E0F0FFC3E1F0FF107C
      C1FF0F7BC1FF0F7BC1FF9FCEE2FF00578FFF5D8BB8FF00578FFF4E8FAAFF7B86
      81FF8A5733FFC8CAC7FFBFDFEFFF0F5480CF0000000000000000001922810086
      9AFE3FBDCDFF75DEEBFF1561A4F7002A80E400606DD2001E2177000000150000
      000000000000000000000000000000000000000000003122107ECD9C66FFE7CB
      B4FFD4A578FFD0A075FFCF9E72FFFBF8F5FFFBF8F5FFCB9E6FFFCB9D6FFFCDA1
      75FFDFC0A5FFB98A59FF27160C7E000000007F4E17CCDBAB47F3FFD464FFFFD5
      65FFFFD567FFFFD25EFFFFCF54FFFFE08AFFD29648FFF1E2D5FFEFDDCFFFEEDB
      CCFFF4E9E0FF22150C6C0000000000000000117CBFFEDDEEF6FF0F7BC1FF0001
      0118000000020001021E0F7BC1FFA6D5EAFF00578FFF5E8CBCFF00578FFF4A8C
      A9FFB6DDEDFFDBEEF6FF4198CDFE01090F490000000200192995004674F40974
      B2FF0088A1FF30ACBCFE19909FF20007083D0000000000000000000000000000
      0000000000000000000000000000000000000000000003020122A67138E5D9B2
      8CFFE6CAB3FFD6A97BFFD1A577FFE2C4A8FFE1C3A8FFD0A274FFD1A475FFDDBD
      A2FFD0AC85FF89512AE502010022000000000805013387561ECC87561ECCDDA1
      53FFDDA153FFEAC064FFFFE496FFDDA153FFE3C89DFFEFDDCFFFEEDBCCFFEEDB
      CCFFF4E9E0FF21150B6B00000000000000001170AAEFCDE5F2FF1A83C3FF0002
      042800000001000509380F7BC1FFA2D3E9FF57AFD9FF00578FFF5E8CBCFF0057
      8FFFA6B5BCFF52A1D1FE0D507BCB00000000000D166F014A7AF40D78BCFF0E7B
      C1FF004876F2005663CE008B9CFF000304290000000000000000000000000000
      0000000000000000000000000000000000000000000000000000160F0753BD81
      3FF4D9B28CFFE6CDB8FFE0BA9DFFD7AB85FFD6A982FFD9B391FFE1C2ABFFD4AE
      86FFA2602FF4120B05530000000000000000000000000000000020140B69FBF7
      F3FFE6AA5CFFFFE599FFE6AA5CFFE8CDA2FFEFDDCFFFEEDBCCFFCD9769FFCD97
      69FFCD9769FF2D1D0F7C0000000000000000031B2A784C9DCFFED4E9F3FF1D85
      C4FF0F7BC1FF0F7BC1FF9FCEE7FF66B5DBFF81C2E1FFCCE6F2FF00578FFF5E8C
      BCFF00578FFF052337AA0000000100000000003E68ED0A70B2FF0E7BC1FF0869
      A9FF003558DA0000000200242882000000130000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000160F
      0853A9763AE6D0A068FFE0BFA0FFE3C5AEFFE3C5AEFFDFBC9FFFC89760FF9964
      2AE6130C055300000000000000000000000000000000000000001F140B68FAF6
      F2FFEEC186FFEDB163FFEBD0A5FFEFDDCFFFEEDBCCFFEEDBCCFFD6AA86FF0000
      00001F140B68020201250000000000000000000000010D5583D23791C8FDBADB
      EDFFD8EBF5FFD6EAF4FFD3E8F3FFCEE6F2FFBCDCEEFF77B5DAFE2386C4FE0057
      8FFF5E8CBCFF00578FFF0000000000000000003B62E50867A6FF096BABFF0042
      6FF5000508440000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000030201233223127D9A6A37DBBA7F3DF3B87E39F393652EDB2F210F7D0302
      01230000000000000000000000000000000000000000000000001F130A67FBF8
      F5FFF9F2EDFFF6EDE6FFF5EAE1FFF4E9E0FFF4E9E0FFF4E9E0FFDCB698FF1F13
      0A670202012500000000000000000000000000000000000000010216226C0F64
      9AE4117ABEFD1179BAFA106FABF00F6599E30B4870C202141F670001021C0000
      000000578FFF00436CDF00000000000000000003063B003559DA003960E30005
      0947000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000110B064D1E13
      0A661E130A661E130A661E130A661E130A661E130A661E130A661E130A660302
      0124000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000080808FF080808FF0808
      08FF080808FF080808FF080808FF080808FF080808FF080808FF080808FF0808
      08FF080808FF080808FF080808FF0000000000000000080808FF080808FF0808
      08FF080808FF080808FF080808FF080808FF080808FF080808FF080808FF0808
      08FF080808FF080808FF080808FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000080808FF131313FF131313FF1313
      13FF131313FF131313FF131313FF131313FF131313FF131313FF131313FF1313
      13FF131313FF131313FF131313FF080808FF080808FF131313FF131313FF1313
      13FF131313FF131313FF131313FF131313FF131313FF131313FF131313FF1313
      13FF131313FF131313FF131313FF080808FF0000000000000000000000000000
      000000000000000000002A8EC8FF2A8EC8FF2A8EC8FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000131313FF171717FF171717FF1717
      17FF131313FF131313FF131313FF131313FF131313FF171717FF171717FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF171717FF131313FF131313FF131313FF131313FF131313FF171717FF1717
      17FF171717FF171717FF171717FF131313FF0000000000000000000000000000
      00000000000000000000389FCFFF04F1F8FF2A8EC8FF00000000000000000000
      000000000000000000000000000000000000A96F4FFFC38E66FFC08B64FFBE88
      62FFBB855FFFB9835DFFB47C5AFFB27A58FFB17956FFAE7755FFAD7454FFAB73
      52FFA97151FFA96F4FFFA96F4FFF00000000131313FF171717FF171717FF1717
      17FF131313FFFFFFFFFFFFFFFFFFC1C1C1FF707070FF131313FF171717FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF171717FF171717FF171717FF171717FF171717FF171717FF171717FF1717
      17FF171717FF171717FF171717FF131313FF0000000000000000000000000000
      00000000000000000000389FCFFF04F1F8FF2A8EC8FF00000000000000000000
      000000000000000000000000000000000000C8926AFF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000A9704FFF00000000131313FF1F1F1FFF1F1F1FFF1F1F
      1FFF171717FFC1C1C1FFC1C1C1FFFFFFFFFFFFFFFFFFD1D1D1FF171717FF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFF131313FF131313FF1F1F1FFF1F1F1FFF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFF131313FF0000000000000000000000000000
      00000000000000000000389FCFFF04F1F8FF2A8EC8FF00000000000000000000
      000000000000000000000000000000000000CA946CFF0000000000000000FFFF
      FEFFFFFFFDFFFEFEFDFFFEFEFCFFFEFEFCFFFEFEFCFFFEFEFCFFFEFEFAFFFEFE
      FAFFFCFCF9FF00000000AA7151FF00000000171717FF1F1F1FFF1F1F1FFF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFF3E3E3EFFD1D1D1FFFFFFFFFF898989FF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFF171717FF171717FF1F1F1FFF1F1F1FFF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F1FFF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFF171717FF0000000000000000000000000000
      0000000000000000000047B0D6FF04F1F8FF2A8EC8FF00000000000000000000
      000000000000000000000000000000000000CC976DFF00000000FFFFFCFFFFFF
      FDFFFEFEFCFFFEFEFCFFFEFEFBFFFDFDFAFFFDFDFAFFFDFDFAFFFDFDFAFFFCFC
      F7FFFBFBF6FF00000000AC7352FF000000001F1F1FFF2B2B2BFF2B2B2BFF2B2B
      2BFF2B2B2BFF6B6B6BFFB7B7B7FFB7B7B7FF898989FFFFFFFFFFF1F1F1FF1F1F
      1FFF2B2B2BFF2B2B2BFF2B2B2BFF1F1F1FFF1F1F1FFF2B2B2BFF2B2B2BFF2B2B
      2BFF2B2B2BFF2B2B2BFF2B2B2BFF2B2B2BFF2B2B2BFF2B2B2BFF2B2B2BFF2B2B
      2BFF2B2B2BFF2B2B2BFF2B2B2BFF1F1F1FFF0000000000000000000000000000
      00000000000047B0D6FF47B0D6FF04F1F8FF2B8FC8FF2B8FC8FF000000000000
      000000000000000000000000000000000000D19C71FF00000000FEFEFCFFFEFE
      FCFFFEFEFCFFFDFDFBFFFDFDFBFFFDFDFAFFFDFDF8FFFBFBF9FFFBFAF7FFFBFA
      F6FFFBF8F4FF00000000B07856FF000000001F1F1FFF2F2F2FFF2F2F2FFF2B2B
      2BFF707070FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF5454
      54FF2F2F2FFF2F2F2FFF2F2F2FFF1F1F1FFF1F1F1FFF333333FF333333FF3333
      33FF333333FF333333FF333333FF333333FF333333FF333333FF333333FF3333
      33FF333333FF333333FF333333FF1F1F1FFF0000000000000000000000000000
      000047B0D6FF47B0D6FF12F1F8FF04F1F8FF04E9F5FF2B8FC8FF2B8FC8FF0000
      000000000000000000000000000000000000D49E73FFA6A6A6FFE1E1E0FFA6A6
      A6FFE0E0E0FFA6A6A6FFE0E0DDFFA6A6A6FFDFDDDBFFA6A6A6FFDFDCD8FFA6A6
      A6FFDFD9D7FFA6A6A6FFB27A58FF000000002B2B2BFF3E3E3EFF3E3E3EFF2F2F
      2FFFE3E3E3FFFFFFFFFFACACACFF2B2B2BFF646464FFFFFFFFFFFFFFFFFF6464
      64FF2F2F2FFF3E3E3EFF3E3E3EFF2B2B2BFF2B2B2BFF373737FF373737FF3737
      37FF373737FF373737FF373737FF373737FF373737FF373737FF373737FF3737
      37FF373737FF373737FF373737FF2B2B2BFF00000000000000000000000047B0
      D6FF47B0D6FF2AEEF8FF12F1F8FF00FFFFFF04E9F5FF0CDCF1FF2B8FC8FF2B8F
      C8FF00000000000000000000000000000000D5A074FF00000000FDFDFCFFFDFD
      FBFFFDFDFAFFFCFCF9FFFCFBF7FFFBF9F5FFFBF8F4FFFBF7F3FFFBF5F2FFFAF3
      EFFFF8F2ECFF00000000B57C5AFF000000002F2F2FFF3E3E3EFF3E3E3EFF2F2F
      2FFFFFFFFFFFFFFFFFFF6B6B6BFF3E3E3EFF2F2F2FFFFFFFFFFFFFFFFFFF5A5A
      5AFF3E3E3EFF3E3E3EFF3E3E3EFF2F2F2FFF333333FF3E3E3EFF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF3E3E3EFF3E3E3EFF3E3E3EFF3E3E3EFF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF3E3E3EFF333333FF000000000000000047B0D6FF47B0
      D6FF48E9F8FF2AEEF8FF0EFFFFFF00FFFFFF00F7FCFF0CDCF1FF19CCECFF2B8F
      C8FF2B8FC8FF000000000000000000000000D8A277FF00000000FDFDFAFFFCFC
      FAFFFCFBF9FFFBFAF6FFFBF8F5FFFBF7F4FFFBF6F1FFF8F4EEFFF7F2EBFFF7F0
      EAFFF6ECE8FF00000000B7815CFF000000003E3E3EFF3E3E3EFF3E3E3EFF3E3E
      3EFFD1D1D1FFFFFFFFFF959595FF3E3E3EFF6B6B6BFFFFFFFFFFF1F1F1FF3E3E
      3EFF3E3E3EFF3E3E3EFF3E3E3EFF3E3E3EFF373737FF3E3E3EFF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF3E3E3EFF3E3E3EFF3E3E3EFF3E3E3EFF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF3E3E3EFF373737FF0000000047B0D6FF47B0D6FF66E3
      F8FF48E9F8FF2AFDFFFF0EFFFFFF00FFFFFF00F7FCFF06E9F8FF19CCECFF25BF
      E8FF2B8FC8FF2B8FC8FF0000000000000000D9A377FF00000000FCFBF9FFFCFB
      F8FFFBF9F7FFFBF7F4FFFAF7F2FFF9F5F0FFF7F3EDFFF6EFEAFFF5EBE7FFF3EA
      E4FFF2E7DEFF00000000BA855EFF000000003E3E3EFF4B4B4BFF4B4B4BFF3E3E
      3EFF7B7B7BFFFFFFFFFFFFFFFFFFCACACAFFF1F1F1FFFFFFFFFF959595FF3E3E
      3EFF4B4B4BFF4B4B4BFF4B4B4BFF3E3E3EFF3E3E3EFF4B4B4BFF4B4B4BFF4B4B
      4BFF4B4B4BFF4B4B4BFF4B4B4BFF4B4B4BFF4B4B4BFF4B4B4BFF4B4B4BFF4B4B
      4BFF4B4B4BFF4B4B4BFF4B4B4BFF3E3E3EFF47B0D6FF47B0D6FF84DDF8FF66E3
      F8FF4CF6FFFF2AFCFFFF0EFFFFFF00FFFFFF00F7FCFF06E9F8FF15D9F4FF25BF
      E8FF31B5E5FF2B8FC8FF2B8FC8FF00000000DBA478FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BD8761FF000000003E3E3EFF545454FF545454FF5454
      54FF3E3E3EFF7B7B7BFFD8D8D8FFFFFFFFFFE3E3E3FF989898FF3E3E3EFF5454
      54FF545454FF545454FF545454FF3E3E3EFF3E3E3EFF545454FF545454FF5454
      54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
      54FF545454FF545454FF545454FF3E3E3EFF47B0D6FF95D9F8FF84DDF8FF66E3
      F8FF49E9F8FF2AEFF8FF12F1F8FF04F1F8FF04E9F5FF0CDCF1FF19CCECFF26BF
      E8FF31B4E5FF38ADE2FF2B8FC8FF00000000DCA779FFDCA779FFDCA779FFDCA7
      79FFDCA779FFDCA779FFDCA779FFDCA779FFDCA779FFDCA779FFDCA779FFDCA7
      79FFDCA779FFDCA779FFC08B64FF000000004B4B4BFF545454FF545454FF5454
      54FF545454FF4B4B4BFF3E3E3EFF3E3E3EFF3E3E3EFF4B4B4BFF545454FF5454
      54FF545454FF545454FF545454FF4B4B4BFF4B4B4BFF545454FF545454FF5454
      54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
      54FF545454FF545454FF545454FF4B4B4BFF61BBDBFF7CC6E0FF7CC6E0FF7CC6
      E0FF7CC6E0FF7CC6E0FF7CC6E0FF7CC6E0FF7CC6E0FF7CC6E0FF7CC6E0FF7CC6
      E0FF7CC6E0FF7CC6E0FF51ADD4FF00000000DDAC85FFE8B992FFE8B992FFE8B9
      92FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B992FFE8B9
      92FFE8B992FFE8B992FFC1906DFF000000004B4B4BFF5A5A5AFF5A5A5AFF5A5A
      5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A
      5AFF5A5A5AFF5A5A5AFF5A5A5AFF4B4B4BFF4B4B4BFF585858FF585858FF5858
      58FF585858FF585858FF585858FF585858FF585858FF585858FF585858FF5858
      58FF585858FF585858FF585858FF4B4B4BFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A96F4FFFDDB18DFFDCA779FFDCA6
      78FFDAA478FFD8A277FFD5A074FFD49E73FFD29D71FFCF9A70FFCE996EFFCB96
      6DFFC9946AFFC49A78FFA96F4FFF00000000000000004B4B4BFF545454FF5454
      54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
      54FF545454FF545454FF4B4B4BFF00000000000000004B4B4BFF545454FF5454
      54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
      54FF545454FF545454FF4B4B4BFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000080808FF080808FF0808
      08FF080808FF080808FF080808FF080808FF080808FF080808FF080808FF0808
      08FF080808FF080808FF080808FF0000000000000000080808FF080808FF0808
      08FF080808FF080808FF080808FF080808FF080808FF080808FF080808FF0808
      08FF080808FF080808FF080808FF0000000000000000080808FF080808FF0808
      08FF080808FF080808FF080808FF080808FF080808FF080808FF080808FF0808
      08FF080808FF080808FF080808FF0000000000000000080808FF080808FF0808
      08FF080808FF080808FF080808FF080808FF080808FF080808FF080808FF0808
      08FF080808FF080808FF080808FF00000000080808FF131313FF131313FF1313
      13FF131313FF131313FF131313FF131313FF131313FF131313FF131313FF1313
      13FF131313FF131313FF131313FF080808FF080808FF131313FF131313FF1313
      13FF131313FF131313FF131313FF131313FF131313FF131313FF131313FF1313
      13FF131313FF131313FF131313FF080808FF080808FF131313FF131313FF1313
      13FF131313FF131313FF131313FF131313FF131313FF131313FF131313FF1313
      13FF131313FF131313FF131313FF080808FF080808FF131313FF131313FF1313
      13FF131313FF131313FF131313FF131313FF131313FF131313FF131313FF1313
      13FF131313FF131313FF131313FF080808FF131313FF171717FF171717FF1717
      17FF131313FF131313FF131313FF131313FF131313FF131313FF171717FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF171717FF131313FF131313FF131313FF131313FF131313FF131313FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF131313FF131313FF131313FF131313FF171717FF171717FF171717FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF171717FF131313FF131313FF131313FF131313FF131313FF131313FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF818181FFE1E1E1FFFFFFFFFFFFFFFFFFC1C1C1FF636363FF171717FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF171717FF545454FFD0D0D0FFFFFFFFFFEFEFEFFFA1A1A1FF242424FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF242424FFEFEFEFFFFFFFFFFF707070FF171717FF171717FF171717FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF242424FF838383FFD0D0D0FFFFFFFFFFE1E1E1FFA1A1A1FF242424FF1717
      17FF171717FF171717FF171717FF131313FF131313FF1F1F1FFF1F1F1FFF1717
      17FF959595FFE1E1E1FFC1C1C1FFD0D0D0FFFFFFFFFFFFFFFFFF636363FF1717
      17FF1F1F1FFF1F1F1FFF1F1F1FFF131313FF131313FF242424FF242424FF1717
      17FF575757FFFFFFFFFFFFFFFFFFC2C2C2FFE1E1E1FFFFFFFFFFD0D0D0FF1717
      17FF242424FF242424FF242424FF131313FF131313FF242424FF242424FF2424
      24FF171717FF858585FFFFFFFFFFE1E1E1FF171717FF242424FF242424FF2424
      24FF242424FF242424FF242424FF131313FF131313FF242424FF242424FF1717
      17FFA1A1A1FFFFFFFFFFF1F1F1FF838383FFC2C2C2FFFFFFFFFFD0D0D0FF1717
      17FF242424FF242424FF242424FF131313FF171717FF1F1F1FFF1F1F1FFF1F1F
      1FFF2F2F2FFF1F1F1FFF1F1F1FFF1F1F1FFF898989FFFFFFFFFFE1E1E1FF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFF171717FF171717FF242424FF242424FF2424
      24FFB5B5B5FFFFFFFFFF898989FF242424FF242424FFE1E1E1FFFFFFFFFF7878
      78FF242424FF242424FF242424FF171717FF171717FF242424FF242424FF2424
      24FF242424FF2F2F2FFFF0F0F0FFFFFFFFFF787878FF242424FF242424FF2424
      24FF242424FF242424FF242424FF171717FF171717FF242424FF242424FF2424
      24FFFFFFFFFFFFFFFFFF696969FF242424FF2F2F2FFFFFFFFFFFFFFFFFFF4B4B
      4BFF242424FF242424FF242424FF171717FF1F1F1FFF2B2B2BFF2B2B2BFF2B2B
      2BFF2B2B2BFF2B2B2BFF2B2B2BFF2B2B2BFF6D6D6DFFFFFFFFFFFFFFFFFF1F1F
      1FFF2B2B2BFF2B2B2BFF2B2B2BFF1F1F1FFF242424FF2B2B2BFF2B2B2BFF2424
      24FFFFFFFFFFFFFFFFFF5F5F5FFF2B2B2BFF2B2B2BFFC2C2C2FFFFFFFFFF8989
      89FF2B2B2BFF2B2B2BFF2B2B2BFF242424FF242424FF2B2B2BFF2B2B2BFF2B2B
      2BFF2B2B2BFF2B2B2BFF8C8C8CFFFFFFFFFFD4D4D4FF2B2B2BFF2B2B2BFF2B2B
      2BFF2B2B2BFF2B2B2BFF2B2B2BFF242424FF242424FF2B2B2BFF2B2B2BFF2B2B
      2BFFE1E1E1FFFFFFFFFF8C8C8CFF2B2B2BFF5F5F5FFFFFFFFFFFFFFFFFFF4444
      44FF2B2B2BFF2B2B2BFF2B2B2BFF242424FF1F1F1FFF2F2F2FFF2F2F2FFF2F2F
      2FFF373737FF636363FF636363FF818181FFE1E1E1FFFFFFFFFFC1C1C1FF2B2B
      2BFF2F2F2FFF2F2F2FFF2F2F2FFF1F1F1FFF242424FF2F2F2FFF2F2F2FFF2B2B
      2BFFFFFFFFFFFFFFFFFFD0D0D0FF5F5F5FFF898989FFFFFFFFFFFFFFFFFF7070
      70FF2F2F2FFF2F2F2FFF2F2F2FFF242424FF242424FF2F2F2FFF2F2F2FFF2F2F
      2FFF2F2F2FFF2F2F2FFF373737FFF0F0F0FFFFFFFFFF626262FF2F2F2FFF2F2F
      2FFF2F2F2FFF2F2F2FFF2F2F2FFF242424FF242424FF2F2F2FFF2F2F2FFF2B2B
      2BFF545454FFF1F1F1FFFFFFFFFFC2C2C2FFFFFFFFFFFFFFFFFFAAAAAAFF2B2B
      2BFF2F2F2FFF2F2F2FFF2F2F2FFF242424FF2B2B2BFF373737FF373737FF2F2F
      2FFF636363FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1F1F1FF575757FF2F2F
      2FFF373737FF373737FF373737FF2B2B2BFF2B2B2BFF373737FF373737FF2F2F
      2FFFD0D0D0FFFFFFFFFFF1F1F1FFFFFFFFFFFFFFFFFFFFFFFFFFACACACFF2F2F
      2FFF373737FF373737FF373737FF2B2B2BFF2B2B2BFF373737FF373737FF3737
      37FF373737FF373737FF2F2F2FFF909090FFFFFFFFFFC9C9C9FF2F2F2FFF3737
      37FF373737FF373737FF373737FF2B2B2BFF2B2B2BFF373737FF373737FF3737
      37FF373737FFBEBEBEFFFFFFFFFFFFFFFFFFFFFFFFFFE1E1E1FF373737FF2F2F
      2FFF373737FF373737FF373737FF2B2B2BFF2F2F2FFF3E3E3EFF3E3E3EFF3E3E
      3EFF2F2F2FFFFFFFFFFFE1E1E1FF959595FF757575FF3E3E3EFF373737FF3E3E
      3EFF3E3E3EFF3E3E3EFF3E3E3EFF2F2F2FFF2F2F2FFF3E3E3EFF3E3E3EFF3737
      37FFA1A1A1FFFFFFFFFFACACACFF707070FF929292FF707070FF373737FF3737
      37FF3E3E3EFF3E3E3EFF3E3E3EFF2F2F2FFF2F2F2FFF3E3E3EFF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF373737FF3E3E3EFFF0F0F0FFFFFFFFFF676767FF3737
      37FF3E3E3EFF3E3E3EFF3E3E3EFF2F2F2FFF2F2F2FFF444444FF444444FF3737
      37FFA1A1A1FFFFFFFFFFD0D0D0FF4B4B4BFFAAAAAAFFFFFFFFFFBEBEBEFF2F2F
      2FFF444444FF444444FF444444FF2F2F2FFF373737FF3E3E3EFF3E3E3EFF3E3E
      3EFF2F2F2FFFFFFFFFFFD0D0D0FF373737FF373737FF373737FF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF3E3E3EFF373737FF373737FF3E3E3EFF3E3E3EFF3E3E
      3EFF545454FFF1F1F1FFFFFFFFFF929292FF373737FF373737FF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF3E3E3EFF373737FF373737FF3E3E3EFF3E3E3EFF3E3E
      3EFF373737FF373737FF373737FF373737FF909090FFFFFFFFFFBDBDBDFF3737
      37FF3E3E3EFF3E3E3EFF3E3E3EFF373737FF373737FF444444FF444444FF3737
      37FFCACACAFFFFFFFFFF959595FF373737FF696969FFFFFFFFFFFFFFFFFF2F2F
      2FFF444444FF444444FF444444FF373737FF3E3E3EFF4B4B4BFF4B4B4BFF4B4B
      4BFF373737FFD8D8D8FFFFFFFFFFCACACAFFCACACAFFCACACAFF898989FF3E3E
      3EFF4B4B4BFF4B4B4BFF4B4B4BFF3E3E3EFF3E3E3EFF4B4B4BFF4B4B4BFF4B4B
      4BFF3E3E3EFF898989FFFFFFFFFFFFFFFFFFF1F1F1FFCACACAFF898989FF3E3E
      3EFF4B4B4BFF4B4B4BFF4B4B4BFF3E3E3EFF3E3E3EFF4B4B4BFF4B4B4BFF3E3E
      3EFFA3A3A3FFC9C9C9FFC9C9C9FFC9C9C9FFD8D8D8FFFFFFFFFFFFFFFFFF5454
      54FF4B4B4BFF4B4B4BFF4B4B4BFF3E3E3EFF444444FF4B4B4BFF4B4B4BFF4444
      44FF7B7B7BFFFFFFFFFFF1F1F1FF959595FFD8D8D8FFFFFFFFFFBEBEBEFF4444
      44FF4B4B4BFF4B4B4BFF4B4B4BFF444444FF3E3E3EFF4F4F4FFF4F4F4FFF4F4F
      4FFF3E3E3EFFCACACAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF989898FF4B4B
      4BFF4F4F4FFF4F4F4FFF4F4F4FFF3E3E3EFF3E3E3EFF545454FF545454FF5454
      54FF545454FF3E3E3EFF545454FFB5B5B5FFE1E1E1FFFFFFFFFF989898FF4B4B
      4BFF545454FF545454FF545454FF3E3E3EFF3E3E3EFF545454FF545454FF3E3E
      3EFFC9C9C9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF7070
      70FF4B4B4BFF545454FF545454FF3E3E3EFF444444FF545454FF545454FF5454
      54FF444444FF7B7B7BFFD8D8D8FFFFFFFFFFF1F1F1FFB1B1B1FF4B4B4BFF4B4B
      4BFF545454FF545454FF545454FF444444FF4B4B4BFF4F4F4FFF4F4F4FFF4F4F
      4FFF4B4B4BFF3E3E3EFF373737FF373737FF373737FF373737FF4B4B4BFF4F4F
      4FFF4F4F4FFF4F4F4FFF4F4F4FFF4B4B4BFF4B4B4BFF545454FF545454FF5454
      54FF545454FF545454FF545454FF4B4B4BFF3E3E3EFF373737FF4B4B4BFF5454
      54FF545454FF545454FF545454FF4B4B4BFF4B4B4BFF545454FF545454FF4B4B
      4BFF3E3E3EFF373737FF373737FF373737FF373737FF373737FF373737FF4B4B
      4BFF545454FF545454FF545454FF4B4B4BFF4B4B4BFF545454FF545454FF5454
      54FF545454FF4B4B4BFF444444FF373737FF444444FF4B4B4BFF545454FF5454
      54FF545454FF545454FF545454FF4B4B4BFF4B4B4BFF575757FF575757FF5757
      57FF575757FF575757FF575757FF575757FF575757FF575757FF575757FF5757
      57FF575757FF575757FF575757FF4B4B4BFF4B4B4BFF575757FF575757FF5757
      57FF575757FF575757FF575757FF575757FF575757FF575757FF575757FF5757
      57FF575757FF575757FF575757FF4B4B4BFF4B4B4BFF585858FF585858FF5858
      58FF585858FF585858FF585858FF585858FF585858FF585858FF585858FF5858
      58FF585858FF585858FF585858FF4B4B4BFF4B4B4BFF585858FF585858FF5858
      58FF585858FF585858FF585858FF585858FF585858FF585858FF585858FF5858
      58FF585858FF585858FF585858FF4B4B4BFF000000004B4B4BFF4F4F4FFF4F4F
      4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F
      4FFF4F4F4FFF4F4F4FFF4B4B4BFF00000000000000004B4B4BFF545454FF5454
      54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
      54FF545454FF545454FF4B4B4BFF00000000000000004B4B4BFF545454FF5454
      54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
      54FF545454FF545454FF4B4B4BFF00000000000000004B4B4BFF545454FF5454
      54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
      54FF545454FF545454FF4B4B4BFF0000000000000000080808FF080808FF0808
      08FF080808FF080808FF080808FF080808FF080808FF080808FF080808FF0808
      08FF080808FF080808FF080808FF0000000000000000080808FF080808FF0808
      08FF080808FF080808FF080808FF080808FF080808FF080808FF080808FF0808
      08FF080808FF080808FF080808FF0000000000000000080808FF080808FF0808
      08FF080808FF080808FF080808FF080808FF080808FF080808FF080808FF0808
      08FF080808FF080808FF080808FF0000000000000000080808FF080808FF0808
      08FF080808FF080808FF080808FF080808FF080808FF080808FF080808FF0808
      08FF080808FF080808FF080808FF00000000080808FF131313FF131313FF1313
      13FF131313FF131313FF131313FF131313FF131313FF131313FF131313FF1313
      13FF131313FF131313FF131313FF080808FF080808FF131313FF131313FF1313
      13FF131313FF131313FF131313FF131313FF131313FF131313FF131313FF1313
      13FF131313FF131313FF131313FF080808FF080808FF131313FF131313FF1313
      13FF131313FF131313FF131313FF131313FF131313FF131313FF131313FF1313
      13FF131313FF131313FF131313FF080808FF080808FF131313FF131313FF1313
      13FF131313FF131313FF131313FF131313FF131313FF131313FF131313FF1313
      13FF131313FF131313FF131313FF080808FF131313FF171717FF171717FF1717
      17FF171717FF171717FF131313FF131313FF131313FF131313FF171717FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF131313FF131313FF131313FF131313FF131313FF131313FF131313FF1313
      13FF131313FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF171717FF131313FF131313FF131313FF131313FF131313FF131313FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF171717FF171717FF171717FF131313FF131313FF131313FF131313FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF171717FF171717FF131313FFFFFFFFFFFFFFFFFF545454FF171717FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF131313FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFF131313FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF171717FF818181FFD0D0D0FFFFFFFFFFFFFFFFFFB1B1B1FF323232FF1717
      17FF171717FF171717FF171717FF131313FF131313FF171717FF171717FF1717
      17FF171717FF171717FF171717FF131313FFFFFFFFFFFFFFFFFF545454FF1717
      17FF171717FF171717FF171717FF131313FF131313FF1F1F1FFF1F1F1FFF1F1F
      1FFF1F1F1FFF1F1F1FFF171717FFFFFFFFFFFFFFFFFF575757FF171717FF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFF131313FF131313FF1F1F1FFF1F1F1FFF1F1F
      1FFF171717FFC2C2C2FFFFFFFFFFFFFFFFFFD0D0D0FFC2C2C2FFC2C2C2FFC2C2
      C2FF171717FF1F1F1FFF1F1F1FFF131313FF131313FF1F1F1FFF1F1F1FFF1F1F
      1FFF171717FF959595FFE1E1E1FFC2C2C2FFE1E1E1FFFFFFFFFFE1E1E1FF1717
      17FF1F1F1FFF1F1F1FFF1F1F1FFF131313FF131313FF1F1F1FFF1F1F1FFF1717
      17FF171717FF171717FF171717FF171717FFFFFFFFFFFFFFFFFF575757FF1717
      17FF1F1F1FFF1F1F1FFF1F1F1FFF131313FF171717FF1F1F1FFF1F1F1FFF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFFFFFFFFFFFFFFFFFF575757FF1F1F1FFF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFF171717FF171717FF1F1F1FFF1F1F1FFF1F1F
      1FFF1F1F1FFF1F1F1FFFA6A6A6FFFFFFFFFFF1F1F1FF4A4A4AFF1F1F1FFF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFF171717FF171717FF1F1F1FFF1F1F1FFF1F1F
      1FFF1F1F1FFF3E3E3EFF1F1F1FFF1F1F1FFF1F1F1FFFE1E1E1FFFFFFFFFF5A5A
      5AFF1F1F1FFF1F1F1FFF1F1F1FFF171717FF171717FF1F1F1FFF1F1F1FFF4A4A
      4AFFC4C4C4FFC4C4C4FFC4C4C4FFC4C4C4FFFFFFFFFFFFFFFFFFD5D5D5FF7878
      78FF1F1F1FFF1F1F1FFF1F1F1FFF171717FF1F1F1FFF2B2B2BFF2B2B2BFF2B2B
      2BFF2B2B2BFF2B2B2BFF1F1F1FFFFFFFFFFFFFFFFFFF5F5F5FFF2B2B2BFF2B2B
      2BFF2B2B2BFF2B2B2BFF2B2B2BFF1F1F1FFF1F1F1FFF2B2B2BFF2B2B2BFF2B2B
      2BFF2B2B2BFF2B2B2BFF2B2B2BFFA9A9A9FFFFFFFFFFF1F1F1FF444444FF2B2B
      2BFF2B2B2BFF2B2B2BFF2B2B2BFF1F1F1FFF1F1F1FFF2B2B2BFF2B2B2BFF2B2B
      2BFF2B2B2BFF2B2B2BFF2B2B2BFF2B2B2BFF323232FFE1E1E1FFFFFFFFFF4444
      44FF2B2B2BFF2B2B2BFF2B2B2BFF1F1F1FFF1F1F1FFF2B2B2BFF2B2B2BFF5F5F
      5FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8C8C
      8CFF2B2B2BFF2B2B2BFF2B2B2BFF1F1F1FFF1F1F1FFF2F2F2FFF2F2F2FFF2F2F
      2FFF2F2F2FFF2F2F2FFF2B2B2BFFFFFFFFFFFFFFFFFF5F5F5FFF2F2F2FFF2F2F
      2FFF2F2F2FFF2F2F2FFF2F2F2FFF1F1F1FFF1F1F1FFF2F2F2FFF2F2F2FFF2F2F
      2FFF2F2F2FFF2F2F2FFF2F2F2FFF2B2B2BFFA9A9A9FFFFFFFFFFD0D0D0FF2B2B
      2BFF2F2F2FFF2F2F2FFF2F2F2FFF1F1F1FFF1F1F1FFF323232FF323232FF3232
      32FF323232FF2B2B2BFF818181FFC2C2C2FFF1F1F1FFFFFFFFFF9C9C9CFF2B2B
      2BFF323232FF323232FF323232FF1F1F1FFF1F1F1FFF303030FF303030FF2B2B
      2BFFD5D5D5FFFFFFFFFF444444FF2B2B2BFFFFFFFFFFFFFFFFFF5F5F5FFF2B2B
      2BFF303030FF303030FF303030FF1F1F1FFF2B2B2BFF373737FF373737FF3737
      37FF373737FF373737FF2B2B2BFFFFFFFFFFFFFFFFFF5F5F5FFF2F2F2FFF3737
      37FF373737FF373737FF373737FF2B2B2BFF2B2B2BFF373737FF373737FF3737
      37FF373737FF373737FF373737FF2F2F2FFF373737FFE3E3E3FFFFFFFFFF8282
      82FF2F2F2FFF373737FF373737FF2B2B2BFF2B2B2BFF3E3E3EFF3E3E3EFF3E3E
      3EFF3E3E3EFF323232FF959595FFFFFFFFFFFFFFFFFFD0D0D0FF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF3E3E3EFF2B2B2BFF2B2B2BFF373737FF373737FF3030
      30FF5F5F5FFFFFFFFFFFBEBEBEFF2B2B2BFFFFFFFFFFFFFFFFFF5F5F5FFF3030
      30FF373737FF373737FF373737FF2B2B2BFF2F2F2FFF3E3E3EFF3E3E3EFF3E3E
      3EFF373737FF2F2F2FFF2F2F2FFFFFFFFFFFFFFFFFFF676767FF373737FF3E3E
      3EFF3E3E3EFF3E3E3EFF3E3E3EFF2F2F2FFF2F2F2FFF444444FF444444FF4444
      44FF444444FF373737FF373737FF444444FF373737FFA6A6A6FFFFFFFFFFBDBD
      BDFF2F2F2FFF444444FF444444FF2F2F2FFF323232FF444444FF444444FF4444
      44FF444444FF3E3E3EFF3E3E3EFF323232FFADADADFFFFFFFFFF959595FF3E3E
      3EFF444444FF444444FF444444FF323232FF303030FF444444FF444444FF4444
      44FF303030FFBEBEBEFFFFFFFFFF676767FFFFFFFFFFFFFFFFFF676767FF3737
      37FF444444FF444444FF444444FF303030FF373737FF3E3E3EFF3E3E3EFF3E3E
      3EFF373737FFD7D7D7FF878787FFFFFFFFFFFFFFFFFF676767FF3E3E3EFF3E3E
      3EFF3E3E3EFF3E3E3EFF3E3E3EFF373737FF373737FF444444FF444444FF4444
      44FF444444FF4F4F4FFF4F4F4FFF373737FF373737FFA9A9A9FFFFFFFFFFBDBD
      BDFF373737FF444444FF444444FF373737FF3E3E3EFF444444FF444444FF4444
      44FF444444FF4F4F4FFF3E3E3EFF3E3E3EFF787878FFFFFFFFFFCACACAFF3E3E
      3EFF444444FF444444FF444444FF3E3E3EFF373737FF444444FF444444FF4444
      44FF444444FF545454FFF2F2F2FFAFAFAFFFFFFFFFFFFFFFFFFF676767FF4444
      44FF444444FF444444FF444444FF373737FF3E3E3EFF4A4A4AFF4A4A4AFF4A4A
      4AFF545454FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF676767FF4A4A4AFF4A4A
      4AFF4A4A4AFF4A4A4AFF4A4A4AFF3E3E3EFF444444FF4A4A4AFF4A4A4AFF4A4A
      4AFF444444FFB0B0B0FFFFFFFFFFCACACAFFD8D8D8FFFFFFFFFFFFFFFFFF8989
      89FF444444FF4A4A4AFF4A4A4AFF444444FF444444FF4B4B4BFF4B4B4BFF4B4B
      4BFF444444FFA3A3A3FFE1E1E1FFCACACAFFF1F1F1FFFFFFFFFFA3A3A3FF4444
      44FF4B4B4BFF4B4B4BFF4B4B4BFF444444FF444444FF4A4A4AFF4A4A4AFF4A4A
      4AFF4A4A4AFF444444FF969696FFFFFFFFFFFFFFFFFFFFFFFFFF676767FF4A4A
      4AFF4A4A4AFF4A4A4AFF4A4A4AFF444444FF3E3E3EFF545454FF545454FF5454
      54FF3E3E3EFF545454FFB1B1B1FFFFFFFFFFFFFFFFFF6F6F6FFF4A4A4AFF5454
      54FF545454FF545454FF545454FF3E3E3EFF444444FF4F4F4FFF4F4F4FFF4F4F
      4FFF4A4A4AFF6F6F6FFFBDBDBDFFF1F1F1FFFFFFFFFFE3E3E3FF989898FF4444
      44FF4F4F4FFF4F4F4FFF4F4F4FFF444444FF444444FF4F4F4FFF4F4F4FFF4F4F
      4FFF4B4B4BFF9C9C9CFFD8D8D8FFFFFFFFFFFFFFFFFFB1B1B1FF4B4B4BFF4B4B
      4BFF4F4F4FFF4F4F4FFF4F4F4FFF444444FF444444FF545454FF545454FF5454
      54FF545454FF545454FF444444FFE5E5E5FFFFFFFFFFFFFFFFFF6F6F6FFF4A4A
      4AFF545454FF545454FF545454FF444444FF4A4A4AFF545454FF545454FF5454
      54FF545454FF545454FF4A4A4AFF373737FF373737FF4A4A4AFF545454FF5454
      54FF545454FF545454FF545454FF4A4A4AFF4A4A4AFF4F4F4FFF4F4F4FFF4F4F
      4FFF4F4F4FFF4F4F4FFF444444FF444444FF373737FF444444FF4A4A4AFF4F4F
      4FFF4F4F4FFF4F4F4FFF4F4F4FFF4A4A4AFF4B4B4BFF4F4F4FFF4F4F4FFF4F4F
      4FFF4F4F4FFF4B4B4BFF444444FF3E3E3EFF3E3E3EFF4B4B4BFF4F4F4FFF4F4F
      4FFF4F4F4FFF4F4F4FFF4F4F4FFF4B4B4BFF4A4A4AFF545454FF545454FF5454
      54FF545454FF545454FF4A4A4AFF444444FF373737FF373737FF4A4A4AFF5454
      54FF545454FF545454FF545454FF4A4A4AFF4A4A4AFF575757FF575757FF5757
      57FF575757FF575757FF575757FF575757FF575757FF575757FF575757FF5757
      57FF575757FF575757FF575757FF4A4A4AFF4A4A4AFF585858FF585858FF5858
      58FF585858FF585858FF585858FF585858FF585858FF585858FF585858FF5858
      58FF585858FF585858FF585858FF4A4A4AFF4B4B4BFF5A5A5AFF5A5A5AFF5A5A
      5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A5AFF5A5A
      5AFF5A5A5AFF5A5A5AFF5A5A5AFF4B4B4BFF4A4A4AFF575757FF575757FF5757
      57FF575757FF575757FF575757FF575757FF575757FF575757FF575757FF5757
      57FF575757FF575757FF575757FF4A4A4AFF000000004A4A4AFF545454FF5454
      54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
      54FF545454FF545454FF4A4A4AFF00000000000000004A4A4AFF4F4F4FFF4F4F
      4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F
      4FFF4F4F4FFF4F4F4FFF4A4A4AFF00000000000000004B4B4BFF4F4F4FFF4F4F
      4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4F
      4FFF4F4F4FFF4F4F4FFF4B4B4BFF00000000000000004A4A4AFF545454FF5454
      54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
      54FF545454FF545454FF4A4A4AFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000080808FF080808FF0808
      08FF080808FF080808FF080808FF080808FF080808FF080808FF080808FF0808
      08FF080808FF080808FF080808FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEFE
      FFFFA6A599FFB1AFACFFDADADAFF000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEFE
      FFFFA6A599FFB1AFACFFDADADAFF00000000080808FF131313FF131313FF1313
      13FF131313FF131313FF131313FF131313FF131313FF131313FF131313FF1313
      13FF131313FF131313FF131313FF080808FF0000000000000000000000000000
      00000000000000000000000000007B441FBB7B441EBB000000007B421DBB7942
      1DBB000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFFFF6B96
      FFFF4192F6FFEEE9DFFFAFADABFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFFFF6B96
      FFFF4192F6FFEEE9DFFFAFADABFF00000000131313FF171717FF171717FF1717
      17FF171717FF131313FF131313FF131313FF131313FF131313FF171717FF1717
      17FF171717FF171717FF171717FF131313FF0000000000000000000000000000
      00000000000000000000000000007B461FBB7B441FBB000000007B431EBB7B43
      1DBB000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFDFFFF6995FFFF419A
      FFFF68DAFFFF5BADF5FFA89F91FF000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDFDFFFF6995FFFF419A
      FFFF68DAFFFF5BADF5FFA89F91FF00000000131313FF171717FF171717FF1717
      17FF171717FF545454FFD0D0D0FFFFFFFFFFE0E0E0FF838383FF171717FF1717
      17FF171717FF171717FF171717FF131313FF0000000000000000000000000000
      00000000000000000000000000007B461FBB7B461FBB000000007B441FBB7B44
      1FBB000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F6F7F6FF5E88FDFF449FFFFF6DDA
      FFFF4EACFFFF5185FFFFF5F6FFFF000000000000000000000000000000000000
      000000000000000000000000000000000000F6F7F6FF5E88FDFF449FFFFF6DDA
      FFFF4EACFFFF5185FFFFF5F6FFFF00000000131313FF1F1F1FFF1F1F1FFF1F1F
      1FFF373737FFF1F1F1FFFFFFFFFFC2C2C2FFF1F1F1FFFFFFFFFF636363FF1717
      17FF1F1F1FFF1F1F1FFF1F1F1FFF131313FF0000000000000000000000000000
      00000000000000000000000000007B4720BB7B461FBB000000007B461FBB7B44
      1FBB000000000000000000000000000000000000000000000000000000000000
      0000E2E3E3FFC7C8C9FFD2D3D4FFF9F9F9FFBFBBB7FF6E91B2FF5FD3FFFF4CAA
      FFFF5888FFFFF8F8FFFF00000000000000000000000000000000000000000000
      0000E2E3E3FFC7C8C9FFD2D3D4FFF9F9F9FFBFBBB7FF6E91B2FF5FD3FFFF4CAA
      FFFF5888FFFFF8F8FFFF0000000000000000171717FF1F1F1FFF1F1F1FFF1F1F
      1FFFA6A6A6FFFFFFFFFFA6A6A6FF1F1F1FFF787878FFFFFFFFFFE0E0E0FF1F1F
      1FFF1F1F1FFF1F1F1FFF1F1F1FFF171717FF0000000000000000000000000000
      00000000000000000000000000007B4920BB7B4920BB000000007B461FBB7B46
      1FBB000000000000000000000000000000000000000000000000EBEBEBFF9799
      9BFFB3A081FFD2B588FFC3AA83FF8F8D87FF707276FFFFF7F0FF6993BDFF5382
      FEFFF9F9FFFF0000000000000000000000000000000000000000EBEBEBFF9799
      9BFFB3A081FFD2B588FFC3AA83FF8F8D87FF707276FFFFF7F0FF6993BDFF5382
      FEFFF9F9FFFF0000000000000000000000001F1F1FFF2B2B2BFF2B2B2BFF2B2B
      2BFFD0D0D0FFFFFFFFFF6D6D6DFF2B2B2BFF333333FFFFFFFFFFFFFFFFFF3333
      33FF2B2B2BFF2B2B2BFF2B2B2BFF1F1F1FFF0000000000000000000000000000
      00000000000000000000000000007B4A20BB7B4920BB000000007B4920BB7B47
      1FBB0000000000000000000000000000000000000000F2F2F2FF98958EFFF5CB
      84FFF5CB84FFF1C885FFF5CE8EFFFCD08CFFAE9E85FF747579FFBFBAAFFFF7F8
      FBFF0000000000000000000000000000000000000000F2F2F2FF98958EFFF5CB
      84FFF5CB84FFC1E0AAFFDAB77CFFFCD08CFFAE9E85FF747579FFBFBAAFFFF7F8
      FBFF000000000000000000000000000000001F1F1FFF333333FF333333FF2B2B
      2BFFFFFFFFFFFFFFFFFF636363FF333333FF2B2B2BFFFFFFFFFFFFFFFFFF6363
      63FF333333FF333333FF333333FF1F1F1FFF0000000000000000000000000000
      0000000000000000000000000011955F2DCC7B4B20BB000000007B4920BB7B49
      20BB0000000000000000000000000000000000000000AEB1B4FFECCB8EFFF3D1
      92FFEECE92FFEDCC8EFFECC784FFEDC687FFFDD28FFF97948EFFFEFEFEFF0000
      00000000000000000000000000000000000000000000AEB1B4FFECCB8EFFF3D1
      92FFD3EDBBFF008100FFC8E2B1FFEDC687FFFDD28FFF97948EFFFEFEFEFF0000
      0000000000000000000000000000000000002B2B2BFF373737FF373737FF2B2B
      2BFFFFFFFFFFFFFFFFFF636363FF333333FF2B2B2BFFFFFFFFFFFFFFFFFF6363
      63FF333333FF373737FF373737FF2B2B2BFF0000000000000000000000000000
      000000000011663D17AAEFB56FFFF1BB7CFF7C4B21BB000000007B4B20BB7B49
      20BB0000000000000000000000000000000000000000A4A098FFFEDFA1FFDACA
      AEFFB2A58FFFB1A189FFB09D83FFAD9775FFC8AA7AFFC1A987FFE2E2E3FF0000
      00000000000000000000000000000000000000000000A4A098FFFEDFA1FFCAE8
      BCFFCFE9CAFF049608FFCFE7C7FFC8E2B1FFDBBB87FFC1A987FFE2E2E3FF0000
      000000000000000000000000000000000000333333FF3E3E3EFF3E3E3EFF3333
      33FFD0D0D0FFFFFFFFFF757575FF373737FF3E3E3EFFFFFFFFFFFFFFFFFF4B4B
      4BFF373737FF3E3E3EFF3E3E3EFF333333FF0000000000000000000000000000
      00007C5022BBF1BE7DFFF3C791FFF2C288FF7C4D21BB000000007C4B21BB7C4B
      20BB0000000000000000000000000000000000000000AAA595FFFFEBB9FF0013
      A4FF00109AFF000C8FFF000883FF000373FFDCBC85FFCFB68DFFD5D5D7FF0000
      00000000000000000000000000000000000000000000AAA595FFC5EBBCFF0091
      00FF049507FF039607FF039506FF008100FFC1E3AFFFCFB68DFFD5D5D7FF0000
      000000000000000000000000000000000000373737FF3E3E3EFF3E3E3EFF3737
      37FFAFAFAFFFFFFFFFFFAFAFAFFF373737FF838383FFFFFFFFFFE0E0E0FF3737
      37FF3E3E3EFF3E3E3EFF3E3E3EFF373737FF0000000000000000000000000000
      0000EFB86EFFF3C892FFF3C892FFF2C288FF7C4E21BB000000007C4C21BB7C4C
      21BB0000000000000000000000000000000000000000ABA9A6FFFFF7CBFFE4E6
      E8FFE4E6E9FFE0DBCBFFDCCFB8FFD8C4A5FFFAD594FFAF9F85FFEEEFEFFF0000
      00000000000000000000000000000000000000000000ABA9A6FFFFF7CBFFD9FD
      DBFFC4F0CAFF049608FFD0EACBFFD3EFBFFFFAD594FFAF9F85FFEEEFEFFF0000
      0000000000000000000000000000000000003E3E3EFF4B4B4BFF4B4B4BFF4B4B
      4BFF545454FFF1F1F1FFFFFFFFFFCACACAFFF1F1F1FFFFFFFFFF898989FF3E3E
      3EFF4B4B4BFF4B4B4BFF4B4B4BFF3E3E3EFF0000000000000000000000000000
      0000B17F3EDDF2C485FFF3C992FFF2C389FF7C4F22BB000000007C4E21BB7C4D
      21BB0000000000000000000000000000000000000000D5D6D7FFD5CCB1FFFFFF
      F2FFFFFFF2FFFBF3D2FFF6E2B4FFF7D99EFFF6D393FFA1A1A4FF000000000000
      00000000000000000000000000000000000000000000D5D6D7FFD5CCB1FFFFFF
      F2FFD9FFDFFF009300FFD7E6C1FFF7D99EFFF6D393FFA1A1A4FF000000000000
      0000000000000000000000000000000000003E3E3EFF545454FF545454FF5454
      54FF3E3E3EFF636363FFD8D8D8FFFFFFFFFFE0E0E0FF989898FF3E3E3EFF5454
      54FF545454FF545454FF545454FF3E3E3EFF0000000000000000000000000000
      00000804013396672FCCF1BA6EFFEFB86EFFEFB76EFFEFB76EFFEFB76EFF7C4E
      21BB0000000000000000000000000000000000000000FEFEFEFFBBBBBBFFD6CF
      B6FFFFFFDCFFFFF6CAFFFFEBB1FFEDD49CFF989590FFEDEDEEFF000000000000
      00000000000000000000000000000000000000000000FEFEFEFFBBBBBBFFD6CF
      B6FFFFFFDCFFC5EFC1FFFFEBB1FFEDD49CFF989590FFEDEDEEFF000000000000
      0000000000000000000000000000000000004B4B4BFF545454FF545454FF5454
      54FF545454FF4B4B4BFF3E3E3EFF373737FF3E3E3EFF4B4B4BFF545454FF5454
      54FF545454FF545454FF545454FF4B4B4BFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFEFFCFCF
      D1FFA3A4A0FFA8A397FF9D9A92FFAAABB0FFF1F1F1FF00000000000000000000
      0000000000000000000000000000000000000000000000000000FEFEFEFFCFCF
      D1FFA3A4A0FFA8A397FF9D9A92FFAAABB0FFF1F1F1FF00000000000000000000
      0000000000000000000000000000000000004B4B4BFF585858FF585858FF5858
      58FF585858FF585858FF585858FF585858FF585858FF585858FF585858FF5858
      58FF585858FF585858FF585858FF4B4B4BFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B4B4BFF545454FF5454
      54FF545454FF545454FF545454FF545454FF545454FF545454FF545454FF5454
      54FF545454FF545454FF4B4B4BFF000000000000000000000000000000000000
      0000A3662BFF0000000000000000000000000000000000000000000000000000
      000000000000A3662BFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D8AB8EFFCD956EFFBD7140FFB766
      33FFB56633FFB46532FFB26432FFB06331FFAE6231FFAC6130FFAA6030FFA95F
      30FFA85E2FFFA75F30FFAB673AFFBC865FFF0000000000000000000000000000
      000000000000A3662BFF00000000000000000000000000000000000000000000
      0000A3662BFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000003020123291A0F7D7D512DDB986135F3975F34F37A4C28DB27180C7D0301
      002300000000000000000000000000000000C37B4DFFEBC6ADFFEAC5ADFFFEFB
      F8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFB
      F8FFFEFBF8FFC89A7AFFC79877FFAD693EFF0000000000000000000000000000
      00000000000000000000A3662BFF00000000000000000000000000000000A366
      2BFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000130C
      085391633EE6D7BBA3FFE9DACAFFECE0D1FFECE0D1FFE8D8C8FFD3B59CFF8757
      2EE6110B0553000000000000000000000000BA6A36FFEDCAB3FFE0A278FFFEFA
      F7FF60C088FF60C088FF60C088FF60C088FF60C088FF60C088FF60C088FF60C0
      88FFFDF9F6FFCA8D63FFC99B7AFFA75F30FF000000000000000000000000A366
      2BFFA3662BFFA3662BFF00000000C19261FFC19261FFC19261FFC19261FF0000
      0000A3662BFFA3662BFFA3662BFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000130E0953AB7B
      51F4E7D5C4FFE5D2BFFFC9A685FFB88E65FFB68A63FFC5A180FFE0CCBAFFE3D0
      BEFF9C653AF4110B06530000000000000000BB6A36FFEECCB6FFE1A278FFFEFA
      F7FFBFDCC2FFBFDCC2FFBFDCC2FFBFDCC2FFBFDCC2FFBFDCC2FFBFDCC2FFBFDC
      C2FFFDF9F6FFCD9066FFCC9E81FFA85F30FF0000000000000000000000000000
      00000000000000000000A3662BFF00000000000000000000000000000000A366
      2BFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000030201229D7654E5EAD8
      C9FFE3CDBAFFC09469FFBA8C60FFCFB094FFCFB094FFB7895DFFB2875FFFDAC0
      AAFFE4D1C0FF8C5D37E50301012200000000BB6936FFEFCEB8FFE1A277FFFEFA
      F7FF60C088FF60C088FFCEF7FFFF272727FF505050FFCEF7FFFF60C088FF60C0
      88FFFDF9F6FFCF9368FFCEA384FFAA5F30FF0000000000000000000000000000
      000000000000A3662BFF00000000000000000000000000000000000000000000
      0000A3662BFF0000000000000000000000000000000A00000000000000000000
      000000000000010D077C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000031261D7EE4CCB9FFEAD6
      C5FFC7996FFFBF9064FFBF9064FFF7F1ECFFF6F0EAFFB7895DFFB7895DFFB589
      61FFE2CEBBFFD9BDA6FF2B1D127E00000000BA6834FFEFD0BBFFE2A278FFFEFB
      F8FFCEF7FFFFCEF7FFFFCEF7FFFFCEF7FFFFCEF7FFFFCEF7FFFFCEF7FFFFCEF7
      FFFFFEFBF8FFD3966BFFD2A78AFFAB6030FF0000000000000000000000000000
      0000A3662BFF000000000000000000000000694C2FC06A4E33BF000000000000
      000000000000A3662BFF00000000000000000000000600000000000000000000
      000000000000115734F207371FCF000000000000000000000004000000000000
      000000000000000000000000000000000000000000009B7B60DBEFE1D3FFD9B5
      95FFC7986AFFC39567FFC19365FFBF9064FFBF9064FFBB8B61FFB98A61FFB88A
      60FFCBA786FFEADCCCFF885F3FDB00000000BB6834FFF0D2BEFFE2A378FFE2A3
      78FFCEF7FFFFCEF7FFFFCEF7FFFF272727FF505050FFCEF7FFFFCEF7FFFFCEF7
      FFFFD8996FFFD6996EFFD5AB8EFFAD6131FF0000000000000000000000000000
      0000000000000000000000000000684A2CC1A96E35FFAA6F36FF6B4E33C00000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001C7B4BF843B27AFD053B21BD0001011D00000000000000000000
      00000000000000000000000000000000000000000000C9A485F6F2E4D9FFD1A5
      78FFC59969FFC49768FFC49667FFFAF6F2FFF3EAE1FFC2956BFFBE8F63FFBE8F
      62FFC0956BFFEFE3D5FFB18259F600000000BB6834FFF2D5C2FFE3A378FFD0F2
      FAFFCEF7FFFFCEF7FFFFCEF7FFFF505050FF272727FF849CA5FFCEF7FFFFCEF7
      FFFFD2F7FEFFD99B71FFDAB095FFAF6231FF0000000000000000000000000000
      00000000000000000000694829C3A96E35FFBE8D5BFFC0905FFFAE743DFF6D50
      33C200000000000000000000000000000000106139D10D6B3CE20E6A3DE00F6A
      3DE010693DDF19834EF55CDA9CFF46B47FFD084B2ABF0001001B000000000000
      00000000000000000000000000000000000000000000D0AC8EF6F2E5DAFFD1A6
      7CFFCC9D6FFFC79A6AFFC59869FFE2CCB6FFF8F3EEFFF6EEE8FFD9BDA1FFC294
      66FFC59B6FFFF0E2D6FFB78B62F600000000BB6834FFF2D8C5FFE3A479FFB5EE
      FEFFB5EFFFFFB5EFFFFFB5EFFFFFB5EFFFFF505050FF272727FF505050FFB5EF
      FFFFB5EFFEFFDC9D72FFDDB59AFFB16332FF0000000000000000000000000000
      0000000000006B4F34BF7A4E24DAAA6F36FFC0905FFFC19261FFB07740FF8057
      2FDA725942BF00000000000000000000000049B27FFD91E3BAFF82DDB0FF7ADC
      ACFF6DD9A4FF5DD49BFF45D08BFF46D18DFF44B980FD0B4C2CB1000000120000
      00000000000000000000000000000000000000000000A98E78DBF3E5D9FFDFBB
      9EFFCFA073FFCD9E70FFF5EBE3FFE4CBB4FFE7D3BFFFFBF8F6FFE5D3BFFFC498
      69FFD6B491FFEEE0D2FF967557DB00000000BB6934FFF4D9C7FFE6A67BFFBAEC
      FAFFB5EFFFFF272727FF505050FFB5EFFFFFB5EFFFFF272727FF272727FFB5EF
      FFFFBAEFFDFFDA9C72FFE1BA9FFFB36432FF0000000000000000000000000000
      0000000000000000000000000000A3662BFFC39565FFC39565FFA96E35FF0000
      00000000000000000000000000000000000052C18CFD9CE6C1FF8AE1B6FF8BE0
      B6FF87E0B5FF86DFB4FF7BDEAEFF82DFB1FF5FC997FD0B5631B1000000120000
      000000000000000000000000000000000000000000003930297EF4E3D4FFEFDC
      CDFFD5A87CFFD0A075FFFBF8F5FFFCF8F5FFFCF8F5FFFBF8F5FFD1A881FFCFA4
      79FFEAD5C3FFEAD4C2FF32281F7E00000000BB6A35FFF4DCC9FFE7A77BFFC9E6
      F0FFB5EFFFFF272727FF272727FFB5EFFFFF849CA5FF272727FF272727FFB5EF
      FFFFC9EFF9FFDEA075FFE4BEA4FFB46532FF0000000000000000000000000000
      0000000000000000000000000000AE743DFFC39565FFC39565FF976637EC0000
      00000000000000000000000000000000000016834CD1159655E2169455E01694
      55E0169354DF25B46CF5ACEACBFF76D7A8FD086C3ABF0002011B000000000000
      0000000000000000000000000000000000000000000004030322C0A690E5F6E9
      DDFFECD8C6FFD7AC81FFDCBB9AFFF6ECE3FFF5ECE2FFE4C8AEFFD2A779FFE6CE
      BAFFF1E2D5FFB09075E50303022200000000BD6C38FFF5DDCCFFE7A87CFFFAF0
      E8FFB5EFFFFF849CA5FF272727FF272727FF272727FF272727FF849CA5FFB5EF
      FFFFF7E5D9FFE0A276FFE7C2A9FFB66633FF0000000000000000000000000000
      00000000000000000000885A2CE5BF8F5DFFC19261FFBD8C59FF775839CB0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000034C67DF88CDEB5FD08743DBF0003011F00000000000000000000
      000000000000000000000000000000000000000000000000000019161353DDC0
      A8F4F7EADFFFEEDED0FFE3C1A7FFD8AE89FFD7AC86FFDDBB9CFFEBD6C7FFF3E6
      D9FFCFAE91F4171310530000000000000000C07240FFF6DFD0FFE8A87CFFFCF6
      F1FFC6F2F6FF94F7FFFF94F7FFFF94F7FFFF94F7FFFF94F7FFFF94F7FFFFB8E4
      E8FFF7E6DBFFE1A378FFEFD5C3FFB76834FF975719FF98581BFF9A5B1EFF9C5D
      20FF9D5E22FFA3662BFFBE8D5BFFC39565FFC6996BFFB7834DFF78634EC00000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000036B879EF197A49C20003021F0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001916
      1453C7AE99E6F9E9DCFFF6E8DDFFF3E5DAFFF3E5DAFFF5E7DCFFF5E4D6FFBFA2
      8CE618141153000000000000000000000000C68253FFF6DFD1FFE9AA80FFFEFA
      F6FFFDFAF6FFDAF1F3FFAFF4FAFF99F6FEFF99F6FEFFAFF4FAFFDAF1F3FFF9EC
      E2FFF8E7DBFFEED0BAFFECD0BDFFBD7241FF98581BFFB7834DFFB8844FFFBA87
      53FFBB8955FFC19261FFC0905FFFC59869FFBB8955FF75583AC8000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000719105701040223000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000040403233B332E7DB69F8EDBDFC3ACF3DDC1AAF3B29B88DB39312B7D0403
      032300000000000000000000000000000000D6A585FFF6E0D1FFF7E0D1FFFEFB
      F8FFFEFBF7FFFDF9F6FFFCF5F0FFFAF0EAFFFBF2EDFFFDF9F6FFFDFAF7FFFBF1
      EBFFF8E9DFFFECD1BEFFCD9268FFE2C5B1FF9B5C1FFF9D5E22FF9E5F23FFA163
      28FFA3662BFFAA6F36FFB7834DFF755F4AC00000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000E1BDA6FFD9AB8DFFC9895CFFC073
      41FFBD6C38FFBB6A35FFBB6934FFBB6834FFBB6834FFBC6A37FFBD6C39FFBB6B
      38FFBF7242FFC98D63FFE7CEBCFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000266C2BFF236727FF1F6223FF1C5E20FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000003020123291B0E7D7E542BDB996431F398622FF37A4E23DB27180B7D0301
      0023000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000002B7331FF72BD78FF70BD76FF206324FF000000000000
      00000000000000000000000000000000000000000000000000000000FFFF0000
      FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000
      FFFF0000FFFF0000FFFF0000000000000000000000000000000000000000130D
      0753906337E6B47D48FFD0A983FFD9B898FFD9B898FFCDA47AFFAC723AFF8655
      26E6110A04530000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000317B37FF77C07CFF74BF7AFF246929FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000130D0853A777
      47F4C6996BFFE1C6ACFFD5B18EFFCAA074FFC99E71FFD5B18EFFE0C4A9FFBF8F
      5DFF97602AF4110A045300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001033
      11B8000000060000000000000000000000000000000000000000000000000000
      0000000000000000000037853DFF7BC282FF78C180FF29702EFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000593410C45C360FC900000000000000000000
      0000000000000000000000000000000000000000000003020122976E44E5C99E
      71FFE1C6ACFFC99E71FFC79B6DFFC6996BFFC49667FFC49667FFC6996BFFE0C4
      A9FFBF8F5DFF855425E502010022000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000010502382161
      25F7164619D90000000000000000000000000000000000000000000000000000
      000000000000000000003D8D44FF81C587FF7CC385FF2F7834FF000000000000
      0000000000000000000000000000000000000000000000000000868688FF8686
      88FF868688FF868688FF694019CFAF763FFFAD733BFF653A11D1868688FF8686
      88FF868688FF868688FF0000000000000000000000002E22167EBF8F5DFFE2C8
      AEFFCCA378FFC99E71FFC99E71FFFEFDFCFFFEFDFCFFC6996BFFC49667FFC699
      6BFFE0C4A9FFAE743DFF27190B7E00000000000000005FBE6BFF5BB866FF56B1
      60FF51A95AFF4BA154FF45994DFF3F9147FF398840FF338039FF3D8843FF57A1
      5CFF428B47FF154017CF00000000000000000000000000000000000000000000
      0000000000000000000043954AFF85C78CFF82C689FF34823BFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000067A4E25D9B58049FFCCA378FFCCA378FFAE743DFF704113DB0000
      000600000000000000000000000000000000000000008E6B49DBD5B18EFFD8B6
      95FFCDA47AFFCAA074FFBF8F5DFFFEFDFCFFFEFDFCFFC6996BFFC6996BFFC79B
      6DFFD5B18EFFCDA47AFF7C5026DB000000000000000063C36FFFA0D7A9FF9CD5
      A5FF98D3A1FF94D09DFF90CE98FF8BCB93FF87C98EFF82C689FF7CC384FF78C1
      80FF74BE7AFF438C48FF133914C4000000000000000000000000000000000000
      00000000000000000000489E51FF8ACA91FF87C98EFF3A8A41FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00005A3D21B8A16B36F7C19261FFCEA67CFFCDA47AFFB17842FF90551CF7512F
      0FBB0000000000000000000000000000000000000000B68C60F6E2C8AEFFD5B1
      8EFFD2AC87FFFEFDFCFFFEFDFCFFFEFDFCFFFEFDFCFFFEFDFCFFFEFDFCFFC699
      6BFFCAA074FFDCBD9FFF9D6631F6000000000000000066C772FFA5DAAEFFA2D8
      ABFF9ED6A7FF9AD4A3FF96D29FFF93CF9AFF8ECC95FF89CA90FF85C78BFF81C5
      87FF7BC282FF48914EFF164019C9000000000000000000000000000000000000
      000000000000000000004EA657FF8ECC95FF8BCB93FF409248FF000000000000
      0000000000000000000000000000000000000000000000000000868688FF8686
      88FF868688FF868688FFB58049FFD0A983FFCFA881FFA26429FF868688FF8686
      88FF868688FF868688FF000000000000000000000000B88E63F6E6CFB8FFDABA
      9AFFD1AB85FFFEFDFCFFFEFDFCFFFEFDFCFFFEFDFCFFFEFDFCFFFEFDFCFFC699
      6BFFCCA378FFDCBD9FFFA06A36F6000000000000000066C772FF66C772FF63C3
      6FFF5FBE6BFF5BB866FF56B160FF51A95AFF4BA154FF45994DFF3F9147FF58A3
      60FF539D5AFF1F4F23D100000000000000000000000000000000000000000000
      0000000000000000000054AD5DFF93CF9AFF90CE98FF469A4EFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B17842FFD2AC87FFD1AB85FFA66A30FF000000000000
      00000000000000000000000000000000000000000000937250DBE2C8AEFFE2C8
      AEFFD3AE8AFFD1AB85FFCAA074FFFEFDFCFFFEFDFCFFC0905FFFC99E71FFC79B
      6DFFD8B695FFD2AC87FF80552DDB000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003B88
      43F72A642FDB0000000000000000000000000000000000000000000000000000
      0000000000000000000059B463FF96D29FFF94D09CFF5BAC63FF030703380000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B7834DFFD3AE8AFFD3AE8AFFAB7038FF000000000000
      0000000000000000000000000000000000000000000030261C7ED3AE8AFFF0E1
      D3FFDDBFA2FFD8B695FFD2AC87FFFEFDFCFFFEFDFCFFD3AE8AFFCDA47AFFCFA8
      81FFE3CAB1FFB7834DFF2A1D0F7E000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002551
      29BB000000060000000000000000000000000000000000000000000000000000
      000035693BBB5AB366F777C683FF9AD4A3FF98D3A1FF7BC386FF499A51F72651
      2AB8000000000000000000000000000000000000000000000000868688FF8686
      88FF868688FF868688FFBE9367FFD2B597FFD1B394FFB68755FF868688FF8686
      88FF868688FF868688FF00000000000000000000000003020222A2815DE5E1C6
      ACFFF1E3D6FFDCBD9FFFD5B18EFFD4B08CFFD9B898FFD8B695FFD7B593FFE4CB
      B3FFC99E71FF90653BE503020122000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000064A9052DB7ACA87FF9ED6A7FF9CD4A5FF71C07BFF3D7C42D90000
      0006000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C29A6FFFD4B89CFFD3B699FFBA8D5FFF000000000000
      000000000000000000000000000000000000000000000000000015110C53B892
      6AF4E2C8AEFFF3E7DCFFE7D1BBFFDFC3A7FFDDBFA2FFE2C8AEFFE9D4C0FFCFA8
      81FFA87849F4130D075300000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000043844BD17CCA88FF79C885FF3C7943CF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C49D73FFD5B99EFFD4B89CFFBD9265FF000000000000
      0000000000000000000000000000000000000000000000000000000000001511
      0C53A4825FE6D8B695FFEAD6C3FFEEDECEFFEBD8C6FFE3CAB1FFC99E71FF9971
      48E6130E09530000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000003E7B46C93A7241C400000000000000000000
      0000000000000000000000000000000000000000000000000000868688FF8686
      88FF868688FF868688FFC59E75FFC59E75FFC29A6FFFC1986CFF868688FF8686
      88FF868688FF868688FF00000000000000000000000000000000000000000000
      00000303022330261B7D937553DBB48D63F3B28A5FF38F6E4BDB2D22177D0302
      0123000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000153D17C4133D16C900000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6A78BEBE7C09FFFE3BC9AFFE0B7
      95FFDDB28FFFD9AE8AFFD6A985FFD3A57DFFD0A079FFCD9C74FFCA9970FFC896
      6CFFC69369FFC49067FFC49066FFA67B57EB0000000000000000000000000000
      00000000000000000000806B5CFF795E49FF7B604BFF806B5CFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001D4C21CF468F4BFF438C48FF164219D1000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EECBABFFE8D5C8FFE8D4C5FFE7D2
      C3FFE5D0C1FFE5CEBEFFE3CCBCFFE3CAB9FFE2C9B7FFE1C7B5FFE0C5B3FFDFC4
      B1FFDFC4B0FFDEC2AFFFDEC2AEFFC49066FF0000000000000000000000000000
      000000000000000000007A5F4AFFECCEB5FFECCEB5FF7A5F4AFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000006275E2CD94F9955FF77C07CFF74BF7AFF448D49FF184A1CDB0000
      0006000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F2D0B1FFEAD8CCFFFCFCFCFFDABA
      A4FFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFCCA182FFFCFCFCFFDFC3B0FFC59268FF0000000000000000000000000000
      00000000000000000000856144FFEFCFB2FFCBAB8EFF856144FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000214B26B837843FF763AD6AFF7BC282FF78C180FF49924EFF236528F71236
      15BB000000000000000000000000000000000000000000000000000000000000
      0006244F28B80000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F5D4B6FFECDCD0FFDEC3AFFFDDBF
      ABFFDBBDA7FFD9BAA3FFD8B79FFFD6B49CFFD5B197FFD3AE94FFD1AB8FFFD0A8
      8CFFCFA688FFCDA385FFE0C5B3FFC7956CFF0000000000000000000000000000
      000000000000000000008E6140FFF6D0AEFFD2AC8AFF8E6140FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000030703384C9A53FF81C587FF7CC385FF2F7834FF000000000000
      0000000000000000000000000000000000000000000000000000000000003B7A
      41D947974FF70307033800000000000000000000000000000000000000000000
      000000000000000000000000000000000000F8D8BBFFEDDED4FFFCFCFCFFFCFC
      FCFFDEC2AEFFFCFCFCFFDBBCA6FFFCFCFCFFD8B69EFFFCFCFCFFD5B097FFFCFC
      FCFFFCFCFCFFFCFCFCFFE1C8B6FFCA9970FF0000000000000000806B5CFF7A5F
      4BFF836046FF8A6141FF8E6140FFD2AC8AFFD4AB8AFF90613FFF8E6140FF8760
      44FF7C6048FF826B5CFF00000000000000000000000000000000000000000000
      0000000000000000000043954AFF85C78CFF82C689FF34823BFF000000000000
      00000000000000000000000000000000000000000000000000003C7943CF6FBE
      79FF78C183FF59AA62FF45994DFF3F9147FF398840FF338039FF2D7633FF286E
      2DFF236727FF1F6122FF1B5C1EFF00000000FADCBFFFEEE0D6FFE3CBBAFFE2C9
      B8FFE0C6B4FFDFC4B1FFDEC2ADFFDCBFAAFFDABCA6FFD9B9A2FFD7B69EFFD6B3
      9AFFE9D6CAFFFCFCFCFFE3CBBAFFCE9D75FF00000000000000007A5F4AFFE6CC
      B4FFD3B59CFFD4B497FFD5B395FFD4B294FFCFAC8BFFCFA989FFCCA688FFC6A4
      87FFDCBEA3FF7C5F4AFF00000000000000000000000000000000000000000000
      00000000000000000000489E51FF8ACA91FF87C98EFF3A8A41FF000000000000
      000000000000000000000000000000000000000000003A7341C479C886FF9CD5
      A5FF98D3A1FF94D09DFF90CE98FF8BCB93FF87C98EFF82C689FF7CC384FF78C1
      80FF74BE7AFF70BD76FF1F6122FF00000000FCDEC1FFEFE2D8FFFCFCFCFFE4CC
      BCFFFCFCFCFFE1C9B7FFFCFCFCFFDFC3B0FFFCFCFCFFDCBEA9FFFCFCFCFFD9B8
      A1FFFCFCFCFFF7F3F0FFE5CEBEFFD1A27BFF00000000000000007A5F4AFFEED4
      BCFFEFD2B7FFF2D2B5FFF1CFB2FFEDCFB2FFEFCFB2FFF5CFB1FFF4CEB0FFEFCD
      B0FFEBCDB2FF7D6149FF00000000000000000000000000000000000000000000
      000000000000000000004EA657FF8ECC95FF8BCB93FF409248FF000000000000
      000000000000000000000000000000000000000000003F7C47C97DCC8AFFA2D8
      ABFF9ED6A7FF9AD4A3FF96D29FFF93CF9AFF8ECC95FF89CA90FF85C78BFF81C5
      87FF7BC282FF76C07CFF236727FF00000000FCDEC1FFEFE2D8FFEFE2D8FFEFE2
      D8FFEEE1D7FFEEE0D6FFEEDFD4FFEDDDD2FFECDCD1FFEBDACEFFEAD8CCFFEAD7
      CAFFE8D5C7FFE7D3C5FFE7D1C2FFD5A883FF0000000000000000806B5CFF7860
      4AFF836046FF8A6141FF8A6141FFEDCFB4FFEDCFB4FF8C6141FF8C6140FF8561
      44FF7D6149FF816D5CFF00000000000000000000000000000000000000000000
      0000000000000000000054AD5DFF93CF9AFF90CE98FF469A4EFF000000000000
      000000000000000000000000000000000000000000000000000044854CD180CD
      8BFF7AC987FF5BB866FF56B160FF51A95AFF4BA154FF45994DFF3F9147FF3988
      40FF338039FF2D7633FF286E2DFF00000000C7AF99E3FCDEC1FFBBB9B6FFB3B1
      AEFFFADBBEFFF8D8BBFFF6D5B7FFF3D2B3FFF0CEAEFFEDC9AAFFEAC5A4FFE7C0
      9FFFE3BC9AFFE0B795FFDDB28FFFAC8A6DE30000000000000000000000000000
      000000000000000000008A6043FFECCEB5FFECCEB5FF8A6043FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000059B463FF96D29FFF94D09CFF4CA255FF000000000000
      0000000000000000000000000000000000000000000000000000000000004B93
      54DB5DB769F70000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C0C0C0FD5D5D
      5DB4000000100000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000836046FFEACEB6FFEACEB6FF836046FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005DBB68FF9AD4A3FF98D3A1FF51AA5BFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0006366B3DBB0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C4C4C4F8BDBD
      BDF81F1F1F670000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007A5F4AFFE8CDB8FFE8CDB8FF7A5F4AFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000061C06DFF9ED6A7FF9CD4A5FF57B261FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000023232367C4C4
      C4F6B6B6B6EFA1A1A1E5898989D86B6B6BC3393939932727277D1C1C1C6E0B0B
      0B49000000040000000000000000000000000000000000000000000000000000
      00000000000000000000806B5CFF7A5F4AFF7A5F4AFF806B5CFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000064C570FFA2D8ABFFA0D7A9FF5BB866FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000066C772FF65C671FF63C26EFF60BE6BFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FCFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000020000
      0008000000080000000800000008000000080000000800000008000000080000
      000800000008000000030000000000000000C76D7CFFC76D7CFFC76D7CFFC76D
      7CFFC76D7CFFC76D7CFFC76D7CFFC76D7CFFC76D7CFFC76D7CFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FCFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000D0D0D51757A78EF8186
      84FC818684FC818684FC818684FC818684FC818684FC818684FC818684FC8186
      84FC818684FC7A7F7DF41618176A00000000C76D7CFFF1D6DAFFEECED3FFEDCB
      D1FFEDCBD1FFEDCBD1FFEDCBD1FFEDCBD1FFF0D3D8FFC76D7CFF000000000000
      00000000000000000000000000000000000000000000BD5969FFBD5969FFBD59
      69FF00000000BD5969FFBD5969FF00000000BD5969FFBD5969FF000000000000
      FCFF0000FCFF0000FCFF0000FCFF0000FCFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000626564D9C8CCCBFFD9DD
      DBFFD9DDDBFFD9DDDBFFD9DDDBFFD9DDDBFFD9DDDBFFD9DDDBFFD9DDDBFFD9DD
      DBFFD9DDDBFFCBCFCEFF7A7E7CF300000000D08390FFF8EAECFFF5E1E4FFF5E1
      E4FFF4DEE2FFF3DCDFFFF3DCDFFFF3DCDFFFF5E1E4FFC76D7CFF000000000000
      00000000000000000000000000000000000000000000BD5969FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FCFF0000000000000000000000005F2F19AED88D5BFEDB90
      5EFFB97044ED3B180C8D000000100501002A4E1E11A2AF5E36EAD67C4FFFD57A
      4BFF6C2D1ABD06010031000000000000000000000000737776ECD5D8D7FFB7BE
      BBFFB6BDBAFFB6BDBAFFB6BDBAFFB6BDBAFFB6BDBAFFB6BDBAFFB6BDBAFFB6BD
      BAFFB7BEBBFFD8DCDBFF858A88FF00000000D28794FFF8EAECFFEFD1D6FFECC8
      CEFFECC8CEFFECC8CEFFEBC6CCFFEDCBD1FFF5E1E4FFC76D7CFF000000000000
      00000000000000000000000000000000000000000000BD5969FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000FCFF0000000000000000000000000000000A0E060344E1A4
      73FFBA774AEC0B04023D0000000000000000010000133011097FDC9669FFDB8F
      5EFF1406035400000002000000000000000000000000737776ECD5D8D7FFBAC0
      BEFFBCC3C0FFBCC3C0FFBCC3C0FFBCC3C0FFBCC3C0FFBCC3C0FFBCC3C0FFBCC3
      C0FFBAC0BEFFD8DCDBFF858A88FF00000000D48C98FFF9EDEFFFF4DEE2FFF2D9
      DDFFF1D6DAFFF1D6DAFFF0D3D8FFF0D3D8FFF4DEE2FFC76D7CFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000014E2A
      179BE2A97AFF48231399000000000000000000000000622F1AB1DFA075FFDA89
      58FF0301002200000000000000000000000000000000737776ECD5D8D7FFF4F5
      F5FFF5F7F7FFF5F8F7FFF6F8F8FFF6F8F8FFF6F8F8FFF6F8F8FFF6F8F8FFF6F8
      F8FFF4F5F5FFD8DCDBFF858A88FF00000000D7939EFFFBF3F4FFEFD1D6FFEBC6
      CCFFECC8CEFFECC8CEFFE9C0C7FFE6B8C0FFECC8CEFFC76D7CFF000000000000
      00000000000000000000000000000000000000000000BD5969FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BD5969FF00000000000000000000000000000000000000000100
      0015B87C4DEAE2A777FF935931D364321BB267331CB5985630D8E3A881FFCF80
      50F90200001B00000000000000000000000000000000737776ECD5D8D7FFF3F4
      F4FFE2E7E4FFE5EAE7FFE8ECEAFFEBEFECFF969997FF747675FF7C807DFFE0E4
      E2FFF3F4F4FFD8DCDBFF858A88FF00000000D998A3FFFCF6F7FFF0D3D8FFE7BB
      C2FFEBC6CCFFEECED3FFE2B3BAFFD1929AFFD895A0FFC97182FF000000000000
      0000000000000000000D010001100000000B00000000BD5969FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BD5969FF00000000000000000000000000000000000000000000
      00001A0D065CCF925FF6925D35D11208044E0502012A502815A0E1A678FFC278
      49F20000000D00000000000000000000000000000000737776ECD5D9D7FFEBEE
      EDFFE1E6E4FFE4E9E6FFE7EBE8FFE9EDEBFF393A3AFFE3E6E4FF292A2AFFD8DD
      DAFFEBEEEEFFDBDEDDFF858A88FF00000000DA9AA5FFFCF9F9FFB6A2B0FFCB8C
      91FFD38A96FFCF9998FF576F98FF5979A3FF946071FFBF5D6DFF000000000000
      0000000000005A5555FF444040FF514043FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000035F381CAAD19562F7200F07660000000A391B0E88E1A677FF9052
      2DD20000000600000000000000000000000000000000737776ECD9DDDBFFE5E7
      E7FFE0E5E2FFE2E7E4FFE4E9E6FFE6EAE8FF9A9C9BFF676A68FF212121FFD7DB
      D9FFE5E7E7FFE0E3E2FF858A88FF00000000DC9FA9FFFEFCFCFF537099FF576F
      98FFB19498FF556F98FF7AC4E7FF537099FF415382FF8D4C60FF000000008F8F
      90FF525252FF595555FF39383AFF615354FF00000000BD5969FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BD5969FF00000000000000000000000000000000000000000000
      00000000000003010022AD794CE2A87246DF160B0554341A0D80E2A97AFF572C
      17A50000000000000000000000000000000000000000747877ECDDE0DFFFE0E3
      E3FFDDE3E0FFE0E5E2FFE1E7E4FFE3E8E5FF9C9F9DFF6C6F6DFF373938FFE2E7
      E4FFE0E3E3FFE5E7E7FF858A88FF00000000DDA2ABFFFEFCFCFF536F98FFCBEF
      F6FF537099FF7AC4E7FF537099FF7AC4E7FF537099FF3C578CFF354B84FF8F8F
      90FF91959BFF595555FF504B4CFF615354FF00000000BD5969FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BD5969FF00000000000000000000000000000000000000000000
      000000000000000000002E1A0C77D49C68F7643E1FAD543019A0E4AE82FF4A26
      14990000000000000000000000000000000000000000747877ECE0E3E2FFD9DD
      DDFFDBE1DEFFDDE3E0FFDEE4E1FFE0E5E2FFDCE2DFFFBABFBCFFD5DAD7FFDFE4
      E1FFD9DDDDFFE9EBEBFF858A88FF00000000E2AEB7FFFEFCFCFFFCFAFAFF556F
      98FFCBEFF6FF537099FF7AC4E7FF537099FF7AC4E7FF5CADEAFF4B99E4FF8F8F
      90FFF1F1F1FF595555FF666261FF695B5DFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000201001C754F2DBACC9866F2CF9B69F5D39A69F74A29
      15980000000800000000000000000000000000000000747877ECE3E5E4FFD5D9
      D9FFD8DFDBFFDAE0DDFFDBE1DEFFDCE2DFFFDDE2DFFF696B6AFF4F5251FFDBE2
      DEFFD5D9D9FFEDEEEEFF858A88FF00000000E2AEB7FFF9EDEFFFF9EDEFFFF8EB
      ECFF546F98FFA5E9F4FF537099FF7AC4E7FF6CBEEFFF5CACE9FF4B99E4FF8F8F
      90FFF1F1F1FF595555FF737070FF747172FF00000000BD5969FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BD5969FF00000000000000000000000000000000000000000000
      00000000000000000000160D0653AA7E50DEE9BC91FFE7BA8FFFE7B78BFFDA9E
      6BFB6C3F20B60000000A000000000000000000000000747877ECE4E7E5FFD7DB
      DBFFD7DADAFFD8DCDBFFD8DCDBFFD8DCDBFFD8DDDCFFD8DDDCFFD8DCDBFFD8DC
      DBFFD9DDDCFFEFF0F0FF858A88FF00000000E2AEB7FFDFA7B0FFDFA7B0FFDFA7
      B0FFDDAAA3FF537099FF97C2D9FFC4EBF6FF6CC0F1FF5CAEEBFF395B9BFF8F8F
      90FFF1F1F1FF595555FF8C8C8CFF8C8C8CFF00000000BD5969FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BD5969FF00000000000000000000000000000000000000000000
      000000000000000000000D0804403A251185482F1894432B148F442914904E30
      179A3D20108A0000000D000000010000000000000000616463D7D4D7D6FFECEE
      EDFFEEF0EFFFF1F2F2FFF3F4F4FFF5F6F6FFF7F7F7FFF7F7F7FFF5F6F6FFF3F4
      F4FFF1F2F2FFDEE0E0FF797D7BF1000000000000000000000000000000000000
      00000000000000000000000000006896C0FF588DC7FF536BA4FF000000008F8F
      90FF8F8F90FF595555FF8C8C8CFF787777FF00000000BD5969FFBD5969FFBD59
      69FF00000000BD5969FFBD5969FF00000000BD5969FFBD5969FF00000000BD59
      69FFBD5969FFBD5969FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000A0B0B49727876EB858A
      88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A
      88FF858A88FF797D7BF113141361000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000665557FF595555FF6B5558FF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E3C1AAFFCD8E
      65FFC06F3EFFBD6935FFBD6935FFBD6935FFBD6835FFBD6834FFBC6833FFBC68
      33FFBC6733FFBE6C3AFFCA8B61FFE4C3AEFF0000000000000000C05F6EFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C05F6EFF0000000000000000000000000000001D000000340000
      0036000000360000003600000036000000360000003600000036082311A11660
      30F5176935FF166030F507201096000000000000000000000000C57B4CFFF9F2
      ECFFF8EDE0FFF6EBDEFFF6EADEFFADF3FBFF009ABBFF009ABBFFADF3FBFFFAF2
      EAFFFCF7F3FFFDF9F5FFFFFFFEFFC3794CFF00000000C05F6EFFC05F6EFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C05F6EFFC05F6EFF000000000000000000000034E5E5E5F5F8F8
      F8FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFF86B196FF268C51FF62BA
      8DFF95D2B2FF62BA8DFF268C51FF071F108C0000000000000000C2753EFFF6EC
      E0FFFDBF66FFFCBD65FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFFCBE5EFFFCBC60FFFEFCF9FFBD6A35FF00000000C05F6EFFC16170FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C16170FFC05F6EFF000000000000000100000036FAFAFAFEFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFF206F3DFF60BA8BFF5EBA
      87FFFFFFFFFF5EB987FF65BC8FFF176333F70000000000000000C37B41FFF7ED
      E3FFFDC26CFFFFD8A0FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFFFD493FFFBBE63FFFBF7F4FFBE6A36FF00000000C26372FFC56978FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C56978FFC26372FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFF2F794AFF9CD4B6FFFFFF
      FFFFFFFFFFFFFFFFFFFF95D2B2FF176935FF0000000000000000C77D43FFE092
      5EFFE08C4AFFF7B454FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFF7B24DFFF7B24DFFFCF9F5FFC2723BFF00000000C36574FFCD7A8AFFC05F
      6EFF0000000000000000000000000000000000000000C05F6EFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C05F6EFF000000000000000000000000000000000000
      0000C05F6EFFCD7A8AFFC36574FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFAFAFAFF488B60FF90D3B1FF92D6
      B1FFFFFFFFFF63BC8CFF65BC8FFF176333F70000000000000000D8813EFFE5A3
      63FFE19156FFFDE5D3FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFFBE0C9FFFBE1C8FFFDFAF7FFC37940FF00000000C36574FFD7939EFFC467
      76FF0000000000000000000000000000000000000000C05F6EFFC05F6EFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C05F6EFFC05F6EFF000000000000000000000000000000000000
      0000C46776FFD7939EFFC36574FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFBFBFBFFA5C4B1FF5FAB81FF95D4
      B4FFBAE6D0FF68BB8FFF2B8F55FF071F108CF2CFB5FFE6A359FFE8AA68FFE39B
      5BFFF9D8C3FFFDE7D6FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFFBE1CBFFFBE1C9FFFBF7F2FFC78144FF00000000C16170FFD6919CFFD48C
      98FFBF5D6DFF00000000000000000000000000000000C05F6EFFC86F80FFC05F
      6EFF000000000000000000000000000000000000000000000000000000000000
      0000C05F6EFFC86F80FFC05F6EFF00000000000000000000000000000000BF5D
      6DFFD48C98FFD6919CFFC16170FF000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFABC8B6FF5E99
      73FF4D8E64FF488A5FFF0D22149800000001E9AC5FFFECB877FFE5A456FFF2D8
      C4FFFEE8D6FFFEE8D7FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFFADFC7FFFADFC6FFFAF2EAFFC88446FF0000000000000000CA7484FFE4B3
      BBFFCF818EFFC05F6EFFC05F6EFFBF5D6DFFBF5D6DFFC05F6EFFCC7887FFC76D
      7CFFC05F6EFF000000000000000000000000000000000000000000000000C05F
      6EFFC76D7CFFCC7887FFC05F6EFFBF5D6DFFBF5D6DFFC05F6EFFC05F6EFFCF81
      8EFFE4B3BBFFCA7484FF00000000000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFAFAFAFFF9F9F9FFF6F6
      F6FFF6F6F6FFFCFCFCFF0000003600000001F4D5B7FFEAAE5FFFEAB56FFFE8A6
      61FFFADBC5FFFEE8D8FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFF9DDC3FFF8DCC2FFFAF4EDFFC88649FF0000000000000000C05F6EFFD58E
      9AFFE2AEB7FFD6919CFFCD7A8AFFCD7A8AFFCE7C8CFFCB7686FFCB7686FFCA74
      84FFC86F80FFC05F6EFF00000000000000000000000000000000C05F6EFFC86F
      80FFCA7484FFCB7686FFCB7686FFCE7C8CFFCD7A8AFFCD7A8AFFD6919CFFE2AE
      B7FFD58E9AFFC05F6EFF00000000000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF6F6F6FFF3F3
      F3FFF2F2F2FFFCFCFCFF00000036000000010000000000000000DF9848FFEBB6
      70FFE8A75FFFFDE7D6FFECB263FFADF3FBFF009ABBFF009ABBFFADF3FBFFF5D6
      BBFFF6DABDFFF6D8BBFFFAF4EFFFC8874AFF000000000000000000000000C05F
      6EFFD38A96FFDEA4AEFFDC9FA9FFD7939EFFD38A96FFD08390FFCC7887FFCA74
      84FFCA7484FFC86F80FFC05F6EFF0000000000000000C05F6EFFC86F80FFCA74
      84FFCA7484FFCC7887FFD08390FFD38A96FFD7939EFFDC9FA9FFDEA4AEFFD38A
      96FFC05F6EFF0000000000000000000000000000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF2F2F2FFEFEF
      EFFFEDEDEDFFFCFCFCFF00000036000000010000000000000000CA8D4DFFE9B1
      6CFFE8AE5DFFFCE6D4FFECB663FFECB663FFADF3FBFFADF3FBFFF5D6BBFFF5D6
      BBFFF3D4B5FFF1D2B3FFF8F4F0FFC6864AFF0000000000000000000000000000
      0000C05F6EFFC76D7CFFD28794FFD58E9AFFD48C98FFD18592FFCE7C8CFFCC78
      87FFC86F80FFC05F6EFF00000000000000000000000000000000C05F6EFFC86F
      80FFCC7887FFCE7C8CFFD18592FFD48C98FFD58E9AFFD28794FFC76D7CFFC05F
      6EFF000000000000000000000000000000000000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFF5F5F5FFF1F1F1FFECECECFFEAEA
      EAFFE6E6E6FFFCFCFCFF00000036000000010000000000000000C88D4FFFF8EF
      E6FFFCE3CFFFFBE4D0FFFCE4CFFFADF3FBFF009ABBFF009ABBFFADF3FBFFF4E9
      DFFFF7F2ECFFFBF7F3FFF5EFE9FFC38146FF0000000000000000000000000000
      00000000000000000000C05F6EFFC16170FFC16170FFC05F6EFFD18592FFC66B
      7AFFC05F6EFF000000000000000000000000000000000000000000000000C05F
      6EFFC66B7AFFD18592FFC05F6EFFC16170FFC16170FFC05F6EFF000000000000
      0000000000000000000000000000000000000000000100000036FCFCFCFFF9F9
      F9FFF9F9F9FFF9F9F9FFF7F7F7FFF6F6F6FFF2F2F2FFEBEBEBFFFCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFF00000036000000010000000000000000C98E51FFF9F5
      F1FFFCE3CDFFFBE3CEFFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFFCE6CDFFFAE5C9FFE2B684FFD6A884FF0000000000000000000000000000
      00000000000000000000000000000000000000000000C05F6EFFC66B7AFFC05F
      6EFF000000000000000000000000000000000000000000000000000000000000
      0000C05F6EFFC66B7AFFC05F6EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFFCFCFCFFF6F6
      F6FFF4F4F4FF3131319100000020000000000000000000000000CA9258FFFBF7
      F3FFFAE0C7FFFBE1C9FFADF3FBFF009ABBFF9BE9F9FF9BE9F9FF009ABBFFADF3
      FBFFF6D8B4FFE1B07BFFDD9668FF000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C05F6EFFC05F6EFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C05F6EFFC05F6EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000036F7F7F7FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFE7E7
      E7FF2F2F2F910000002000000002000000000000000000000000D2A272FFF8F3
      EEFFF9F5EFFFF8F4EDFFF8F3EDFFADF3FBFF009ABBFF009ABBFFADF3FBFFF2E6
      D7FFE2B27BFFDD996AFF00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000C05F6EFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C05F6EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000033DBDBDBF0F7F7
      F7FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF2F2F
      2F91000000200000000200000000000000000000000000000000E8CFB9FFD7AA
      7BFFCC9459FFCA9154FFCA9053FFCA9053FFCA9153FFCB9053FFC98F53FFCF9D
      68FFDEB290FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      002000000002000000000000000000000000000000000000001D000000340000
      0036000000360000003600000036000000360000003600000036000000360000
      003600000036000000330000001D000000000000000000000000000000000107
      0A37000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000C494
      5EFDCA9863FFCA9763FFCA9763FFCA9763FFCA9762FFC99762FFC99762FFCA98
      63FFC4935EFD00000000000000000000000000000000858887FF858A88FF858A
      88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF949695FF000000000000000000000034E5E5E5F5F8F8
      F8FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFF8F8F8FDE1E1E1F300000033000000000000000000000000000000001C77
      9AD51F8DB7E90000000C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000005C5C5CC2575757DA565656FFC795
      5FFFF9F7F6FFF9F1ECFFF9F1EBFFF8F0E9FFF7EDE6FFF4EAE1FFF2E8DEFFFAF8
      F6FFC7945FFF222222FF353535D9535353BF898C8BFFBBBCBBFFE5E5E5FFE2E2
      E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2E2FFE2E2
      E2FFE2E2E2FFE1E2E2FFB6B7B7FF878A88FF0000000100000036FAFAFAFEFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFF8F8F8FD00000036000000000000000000000000000000000000
      000C2BAEDFFF29ABDEFF0A303F8A000000000000000000000000000000000000
      000000000000000000000000000000000000676767FDA7A7A7FFB5B5B5FF8181
      81FFAFACAAFFC5C0BDFFC5C0BDFFC5C0BDFFC5C0BDFFC5C0BDFFC5C0BDFFADAA
      A8FF2A2A2AFFB5B5B5FF9B9B9BFF212121FF858A88FFEBEBEAFFD0CFCFFFCFCF
      CFFFCBCBCBFFCCCBCBFFCBCCCCFFCCCBCCFFCCCCCCFFD0CFD0FFD0CFD0FFD0CF
      D0FFD0D0D0FFD3D4D3FFEBEAEAFF858A88FF0000000100000036FCFCFCFFFCFC
      FCFF164158FF295F89FF4A8ABEFF6DA8CBFFE0E9F1FFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFCFCFCFF00000036000000010000000000000000000000000000
      00001B7393CF4DBCE7FF4ABAE6FF2090BDED0001011700000000000000000000
      0000000000000000000000000000000000006E6E6EFFB5B5B5FFB5B5B5FF9595
      95FF818181FF818181FF777777FF6C6C6CFF5F5F5FFF505050FF414141FF4040
      40FF6C6C6CFFB5B5B5FFB5B5B5FF232323FF858A88FFFFFFFFFFD6D6D6FF9696
      96FF959696FF969696FF969596FF969696FF959596FF969696FF969696FF9696
      95FF959696FFD6D5D6FFEFEEEEFF858A88FF0000000100000036FCFCFCFFFCFC
      FCFF2C6585FF94C7F9FF91C9F9FF3F85C9FF2469AEFFD4E2EEFFFAFAFAFFFAFA
      FAFFFAFAFAFFFCFCFCFF00000036000000010000000000000000000000000000
      00000000000029AFDFFF84D3F2FF53BDE7FF2CAADEFF0B334590000000000000
      000000000000000000000000000000000000737373FFBBBBBBFFBBBBBBFF8D8D
      8DFFD4D4D4FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9B9FFB9B9B9FFD3D3
      D3FF838383FFBBBBBBFFBBBBBBFF282828FF858A88FFFFFFFFFFDDDDDCFFDDDD
      DDFFDDDCDDFFDDDDDDFFDDDDDCFFDDDCDDFFD8D8D8FFD7D7D8FFD8D8D8FFD8D8
      D8FFD8D8D8FFDDDDDDFFF3F3F3FF858A88FF0000000100000036FCFCFCFFFCFC
      FCFF4189AAFFE0F2FFFF529AD8FF1878BEFF4798C5FF468EC7FFD8E6F3FFF8F8
      F8FFF8F8F8FFFCFCFCFF00000036000000010000000000000000000000000000
      00000000000018657EC06ECCEEFF83D2F2FF7CCEF1FF48B6E4FF1F93C3F10001
      011700000000000000000000000000000000787878FFD7D7D7FFD7D7D7FF9797
      97FFD8D8D8FFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFD7D7
      D7FF8E8E8EFFD7D7D7FFD7D7D7FF3D3D3DFF858A88FFFFFFFFFFE3E3E3FFA0A0
      A0FFA0A0A0FFA0A0A0FFA0A0A0FFE3E3E3FFA1A1A1FFA1A1A1FFA1A1A1FFA1A1
      A1FFA1A1A1FFE3E3E3FFF9F9F9FF858A88FF0000000100000036FCFCFCFFFCFC
      FCFFA5C3D7FF78B6D5FF90B7D1FF53C9E4FF59DFF5FF76D0EDFF4F9DDDFFDFEB
      F5FFF8F8F8FFFCFCFCFF000000360000000130ABCCF032B5D9F831B3D8F82FB1
      D7F82DAED6F82BACD5F886D7F3FF2DB6EBFF48BCECFF80CEF1FF4FB9E6FF2AA8
      DDFF0C3C529D0000000000000000000000007C7C7CFFF9F9F9FFF9F9F9FFABAB
      ABFFDFDFDFFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFCBCBCBFFDFDF
      DFFFA3A3A3FFF9F9F9FFF9F9F9FF5F5F5FFF858A88FFFFFFFFFFE3E3E3FFA0A0
      A0FFBEBEBEFFBEBEBEFFA0A0A0FFE3E3E3FFE3E3E3FFE3E3E3FFE3E3E3FFE3E3
      E3FFE3E3E3FFE3E3E3FFFFFFFFFF858A88FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFB2D5E5FF74BAD7FFC2F6FDFF61DFF7FF5BE2F8FF77D3F0FF4798
      DCFFDEE9F2FFFCFCFCFF00000036000000012FA3C1E973DAF2FF93E6F8FF91E3
      F7FF8DE0F6FF8ADCF5FF8ADBF5FF88D7F4FF84D3F2FF7DCFF1FF7ACCF0FF78C9
      EFFF46B4E3FF1F96C8F500020323000000007E7E7EF9FCFCFCFFFCFCFCFFCBCB
      CBFFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2
      F2FFC6C6C6FFFCFCFCFFFCFCFCFF6F6F6FFE858A88FFFFFFFFFFE9E9E9FFA0A0
      A0FFBEBEBEFFBEBEBEFFA0A0A0FFE9E9E9FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
      A0FFA0A0A0FFE9E9E9FFFFFFFFFF858A88FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFAFD4E5FF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF499ADDFFD4E5F5FF000000360000000102090A374DCDECFF98E9F9FF48D5
      F3FF43CFF1FF3ECAF0FF36C2EEFF89D9F4FF2CB2DFFE28A7D3F827A4D3F825A2
      D2F8239FD0F8219CCFF81C88B6E9000000006E6E6EDAD2D2D2FFE8E8E8FF7B7B
      7BFF7B7B7BFF7B7B7BFF7B7B7BFF7B7B7BFF7B7B7BFF7B7B7BFF7B7B7BFF7B7B
      7BFF7B7B7BFFE8E8E8FFC4C4C4FF535353E1858A88FFFFFFFFFFE9E9E9FFA0A0
      A0FFBEBEBEFFBEBEBEFFA0A0A0FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFFFFFFFFF858A88FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFBDE5F2FF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF4FA2E2FF04090D5C000000010000000035B6D5F480E1F5FF8EE6
      F8FF41D2F3FF3DCDF1FF37C7EFFF8CDCF5FF56C6EAFF09252E74000000000000
      0000000000000000000000000000000000008E8E8ECD9A9A9AFFCCCCCCFFC78B
      4CFFF9F4EDFFFEE8D8FFFEE8D7FFFDE5D3FFFCE4D1FFFAE0C7FFF9DDC3FFFAF4
      EDFFC78548FFC3C3C3FF727272FF848484CD858A88FFFFFFFFFFE9E9E9FFA0A0
      A0FFA0A0A0FFA0A0A0FFA0A0A0FFE9E9E9FFA0A0A0FFA0A0A0FFA0A0A0FFA0A0
      A0FFA0A0A0FFE9E9E9FFFFFFFFFF858A88FF0000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFBAE3F0FF7BD4EEFFC4F6FDFF6ADD
      F6FF6BCAEDFF61A3D7FF5087B7F001030427000000000615195459D4EFFF99EA
      F9FF45D6F4FF40D0F2FF3BCBF0FF6CD5F3FF7DD7F3FF48C0E7FF030F124A0000
      00000000000000000000000000000000000000000000767676C27B7B7BF4C589
      4AFFF9F4EFFFFEE7D7FFFDE7D5FFFCE6D2FFFBE1CCFFF8DCC2FFF6DABDFFFAF4
      EFFFC48346FF575757F46D6D6DC200000000858A88FFFFFFFFFFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFFFFFFFFF858A88FF0000000100000036FCFCFCFFF9F9
      F9FFF9F9F9FFF9F9F9FFF7F7F7FFF6F6F6FFF2F2F2FFA8D9E8FF81D6EEFFB2E3
      F9FF8BC0E7FFAED3F6FFC4E0FCFF5E95C5F7000000000000000039C0DEF993E9
      F9FF70E1F7FF43D4F3FF3FCEF2FF3AC9F0FF8ADCF5FF6ED0EFFF3BBBE4FF0003
      042300000000000000000000000000000000000000000000000000000000BC86
      49F9F9F4F0FFFCE6D3FFFDE7D3FFFBE3CDFFFAE0C8FFF5D6BBFFF3D4B5FFF8F4
      F0FFBA7F45F9000000000000000000000000858A88FFFFFFFFFFE9E9E9FF6A6A
      6AFF6A6A6AFF6A6A6AFF6A6A6AFF6A6A6AFF6A6A6AFFE9E9E9FFE9E9E9FFA0A0
      A0FFA0A0A0FFE9E9E9FFFFFFFFFF858A88FF0000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFAFE4F3FF75BE
      E7FFB4D2F0FFE5F3FFFFACD2EFFF3A73A4E8000000000000000010353D8362D9
      F1FF9AEBFAFF46D8F4FF42D3F3FF3DCEF1FF38C8F0FF8CDCF5FF60CBEDFF2FB4
      DDFC00000000000000000000000000000000000000000000000000000000BB85
      4BF7F9F5F1FFFCE3CFFFFCE4CFFFFAE1CAFFF9DDC4FFF4E9DFFFF7F2ECFFF5EF
      E9FFBD7B44FB000000000000000000000000858A88FFFFFFFFFFE9E9E9FF6A6A
      6AFF6A6A6AFFA0A0A0FF6A6A6AFF6A6A6AFFA0A0A0FFE9E9E9FFE9E9E9FFA0A0
      A0FFA0A0A0FFE9E9E9FFFFFFFFFF858A88FF0000000000000036F7F7F7FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFACD5
      E4FF56A5D8FF85B1DBFF449DD0FF05141C5E0000000000000000000000003DC8
      E7FD9AEDFAFF99EBF9FF97E8F9FF94E5F8FF91E2F7FF8EDFF6FF8BDBF5FF54C7
      EBFF2BA7CDF3000000000000000000000000000000000000000000000000BA83
      4AF6F9F5F1FFFCE3CDFFFBE3CDFFF9E0C8FFF8DCC2FFFDFBF8FFFCE6CDFFE2B6
      84FF7E644EC5000000000000000000000000858A88FFFFFFFFFFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9E9FFE9E9
      E9FFE9E9E9FFE9E9E9FFFFFFFFFF858A88FF0000000000000033DBDBDBF0F7F7
      F7FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF2F2F
      2F91000000200000000200000000000000000000000000000000000000001952
      5EA23DCCEBFF3CCBEAFF3AC9E9FF39C7E9FF38C4E8FF36C2E7FF34C0E6FF33BD
      E5FF31BBE4FF258FB0E10000000000000000000000000000000000000000BD82
      49FAF7F2ECFFF8F4EEFFF8F3EDFFF8F3EDFFF8F2ECFFF2E6D7FFE2B27BFFCB8B
      60F600000000000000000000000000000000858A88FFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFF858A88FF000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      0020000000020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008778
      6CC38A6D4FCDC68A4EFEC88C4DFFBD884FF7BE874FF7C3874BFE7E6450C10000
      000000000000000000000000000000000000858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF858A88FF858A88FF00000000000000000000000039A8
      DBFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000039A8DBFF39A8
      DBFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000303
      0F41000000080000000000000000000000000000000000000000000000000000
      000801010E410000000000000000000000000000000039A8DBFF39A8DBFF39A8
      DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8
      DBFF39A8DBFF39A8DBFF0000000000000000000000000C0C0CFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000003A3A3AFF3333
      33FF2D2D2DFF272727FF222222FF1C1C1CFF171717FF121212FF0D0D0DFF0000
      000000000000000000000000000000000000000000000000000004040F414D4A
      F2FF3E3CE9FD0000000800000000000000000000000000000000000000082220
      DEFC2F2DEAFF01010E4100000000000000000000000039A8DBFF39A8DBFF39A8
      DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8
      DBFF39A8DBFF39A8DBFF000000000000000000000000141414FF00000000E3AA
      83FFE1A57BFFDE9F75FFDC9B70FFDA966AFFD89065FFD68C60FFD5865BFFD485
      58FFD28356FFD28356FFD28356FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000005050F415654F5FF615F
      FAFF5653F6FF3E3DE7FC000000080000000000000000000000082A28E0FC3F3D
      F1FF4A48F6FF2F2DEAFF01010E4100000000000000000000000039A8DBFF39A8
      DBFF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000001D1D1DFF00000000E5B0
      89FFEAC0A2FFE9BD9EFFE8B99AFFE6B596FFE4B192FFE3AD8DFFE1A989FFDFA5
      86FFDEA383FFDDA080FFD28356FF000000000000000000000000484848FF4242
      42FF3C3C3CFF363636FF303030FF2A2A2AFF242424FF1E1E1EFF191919FF1414
      14FF0F0F0FFF0A0A0AFF0000000000000000000000000202062B5956F6FF6360
      FAFF6F6EFFFF5754F6FF3F3EE8FC00000008000000083330E3FC4543F2FF6160
      FFFF4846F4FF2D2BE9FF0000062B0000000000000000000000000000000039A8
      DBFF000000003E3933FF48423CFF47423CFF38342FFF282421FF000000003532
      32FF262320FF22201DFF181614FF030202FF00000000292929FF00000000E6B3
      8CFFECC4A6FFEBC2A3FFEABFA0FFE8BC9DFFE7B899FFE5B494FFE4AF91FFE2AC
      8CFFE1A888FFDFA585FFD48558FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000202062B5957
      F6FF6461FAFF726FFFFF5856F6FF3F3EE8FC3C3AE8FD4E4BF4FF6665FFFF4E4C
      F5FF3432EBFF0101062B00000000000000000000000000000000000000000000
      00000000000047413BFF90857AFFABA197FF7A7066FF47413CFF000000004642
      3FFFA09488FF9F9489FF7B7167FF171513FF00000000343434FF00000000E6B3
      8CFFECC5A7FFECC5A7FFEBC4A5FFEBC1A3FFEABEA0FFE8BA9CFFE6B697FFE4B3
      93FFE3AF8FFFE1AA8BFFD68C60FF0000000000000000000000009E6B42D40000
      000000000000000000003C3C3CF8363636F8303030F82A2A2AF8242424F81F1F
      1FF81B1B1BF8161616F800000000000000000000000000000000000000000202
      062B5A58F6FF6562FAFF7270FFFF716EFFFF6E6CFFFF6C6AFFFF5553F7FF3D3B
      EEFF0101062B0000000000000000000000000000000000000000000000000000
      000000000000504A44FF90867BFFAEA59BFF756B62FF534E4AFF000000004340
      3DFF9A8F84FFA0978CFF786F65FF1D1A18FF00000000414141FF00000000E6B3
      8CFFE6B38CFFE6B38CFFE6B38CFFE6B38CFFE5B089FFE3AC85FFE2A980FFE1A3
      79FFDE9E73FFDC996EFFDA9468FF0000000000000000000000009F6F48D49C65
      3BD60502012900000000303030D42C2C2CD4282828D4232323D41F1F1FD41B1B
      1BD4181818D4131313D400000000000000000000000000000000000000000000
      00000202062B5B59F7FF7774FFFF5754FFFF5552FFFF706EFFFF4644F0FF0101
      062B000000000000000000000000000000000000000000000000000000000000
      00000000000076706CFFA3978CFFC2B9AFFFA6998EFF827A74FF000000006D6A
      67FF9C9186FFBBB0A5FFA69A8EFF4D4A47FF000000004E4E4EFF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E7B283FFEDBD97FFE8B4
      89FF8C5A34CB0502012900000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000085956F2FD7B77FFFF5C59FFFF5956FFFF7472FFFF4441EBFD0000
      0008000000000000000000000000000000000000000000000000000000000000
      0000000000005E5751FF443F39FF4C4640FF36312DFF363330FFC6C6C5FF5856
      54FF1B1917FF1E1B19FF110F0EFF0F0F0FFF000000005A5A5AFF00000000F5C5
      A7FFF5C3A3FFF5C09FFFF5BE9BFFF3BB99FFF3B895FFF3B792FFF3B690FFF3B6
      8EFF000000000000000000000000000000000000000000000000A17857D49E70
      47D50503012900000000505050F84A4A4AF8444444F83E3E3EF83A3A3AF83434
      34F82E2E2EF8282828F800000000000000000000000000000000000000000000
      0008625FF3FC6E6BFBFF807CFFFF7C79FFFF7A77FFFF7775FFFF5C5AF7FF4340
      E9FC000000080000000000000000000000000000000000000000000000000000
      000000000000A3978BFFA99D90FF7D7369FF7D7267FF7B7065FF4C453DFF695E
      54FF84766BFF786C62FF756B61FF1A1816FF00000000666666FF00000000F5C8
      ABFFF7D3BCFFF7D2BAFFF7CFB6FFF7CDB4FFF6CBB1FFF6CAAEFFF6C8ACFFF3B6
      90FF000000000000000000000000000000000000000000000000A37C5DD40000
      00000000000000000000404040D73D3D3DD7393939D7343434D72F2F2FD72B2B
      2BD7272727D7232323D700000000000000000000000000000000000000086A67
      F6FC7572FDFF8682FFFF7471FCFF6260F8FF5E5BF7FF6B68FAFF7977FFFF5E5B
      F7FF4441E9FC0000000800000000000000000000000000000000000000000000
      000000000000C8BFB7FFA89B90FF897C72FF867668FF8E7D6EFFA19181FF9583
      70FF937D6BFF897464FF7B6F64FF53514FFF00000000727272FF00000000F5CB
      AEFFF7D5C0FFF7D3BDFFF7D2BAFFF7D0B8FFF7CEB4FFF6CCB2FFF6CBAFFFF3B8
      94FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000008706DFAFD7B78
      FEFF8A87FFFF7A77FDFF6A67FBFF0202062B0202062B5F5CF8FF6C6AFAFF7B78
      FFFF5F5DF7FF4542EAFC00000005000000000000000000000000000000000000
      00000000000000000000897C71FF9B8E82FF887B70FF544D46FFC0BCB8FF5B54
      4CFF857466FF7A6D61FF6A635DFFBBBAB9FF000000007D7D7DFF00000000F5CB
      AFFFF7D7C2FFF7D5C0FFF7D4BEFFF7D2BBFFF7D1B8FFF7CFB5FFF6CDB3FFF3BB
      96FF000000000000000000000000000000000000000000000000747474FF7272
      72FF6E6E6EFF6A6A6AFF666666FF616161FF5C5C5CFF575757FF515151FF4C4C
      4CFF464646FF404040FF0000000000000000000000000101031F7875FFFF817C
      FFFF817CFEFF726FFDFF0302072B00000000000000000202062B605DF8FF6D6B
      FBFF7C7AFFFF605DF8FF0D0D2D6F000000020000000000000000000000000000
      00000000000000000000E5E3E1FFCFC9C4FFACA297FF756D64FF00000000756B
      62FFA09488FFBAB3ADFFE2E1E0FF0000000000000000898989FF00000000F5CB
      AFFFF5CBAFFFF5CBAFFFF5C9ACFFF5C8AAFFF5C5A7FFF5C3A3FFF5C09FFFF5BE
      9BFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000101031F7875
      FFFF7774FEFF0303072B000000000000000000000000000000000202062B625F
      F8FF6866F9FF242269A801010629000000000000000000000000000000000000
      0000000000000000000000000000DFDBD7FFB3A79CFFA5998DFF00000000AC9F
      92FF94897DFFB8B4B0FF000000000000000000000000919191FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000101
      031F0303072B0000000000000000000000000000000000000000000000000202
      062B11102E6F05040E3E00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000A00000000000000000000000000000000C6BFF8FFC6BFF8FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000114E290B99D96E1DFFD96C1AFF894410CC2E1605772E15
      0577B95616EEB95416EE4B220899000000110000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000039A8DBFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6BFF8FF7363EEFF6D5CEDFF9387
      F2FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000023130666E18F4AFFEBB183FFE3914CFFDA7121FFDA7226FFE088
      46FFE7A26EFFE3935AFFDB7331FF0F0601440000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000039A8DBFF39A8DBFF00000000000000000000000039A8DBFF39A8DBFF0000
      000039A8DBFF39A8DBFF000000000000000039A8DBFF000000000000000039A8
      DBFF000000000000000039A8DBFF00000000C6BFF8FF6D5CEDFF6D5CEDFF6D5C
      EDFF9B80C8FFE2C0AAFF00000000000000000000000000000000E2C0AAFFE2C0
      AAFF000000000000000000000000000000000000000000000000000000000000
      0000000000000F090344E28D42FFECB588FFDC782AFF0000001103010022A352
      15DDE6A46FFFDC7832FF0F07014400000000000000000000000039A8DBFF39A8
      DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8
      DBFF39A8DBFF39A8DBFF39A8DBFF0000000000000000000000000000000039A8
      DBFF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009387F2FF6D5CEDFF6245
      CEFF6042C9FF8C5C9AFF00000000000000000000000000000000CE946CFFBE6E
      3CFFC17544FF0000000000000000000000000000000000000000000000000000
      000000000000000000008E521FCCE69957FFE49753FF2F1A0877000000002F18
      0677E18C4AFFE18B47FF2E17057700000000000000000000000039A8DBFF39A8
      DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF39A8
      DBFF39A8DBFF39A8DBFF39A8DBFF0000000000000000000000000000000039A8
      DBFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000ABA1F5FF6041
      C8FF6041C8FF6750DDFFABA1F5FF00000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF00000000000000000000000000000000000000000000
      000000000000000000000000001177461BBBE59854FFE59551FF8D4E1ACC6134
      10AAE69D61FFE8A46BFF8A4612CC000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000039A8DBFF39A8DBFF000000000000000000000000000000000000000039A8
      DBFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008853
      8EFF6041C8FF6750DDFF6D5CEDFFABA1F5FF000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF00000000000000000000000000000000000000000000
      00000000000033753AE537853DFF2F7835FF7D7830FF91752CFC774519BBDF7C
      2AFFE3924EFFEBB386FFDC7526FF00000011030202FF181614FF22201DFF2623
      20FF353232FF00000000282421FF38342FFF47423CFF48423CFF3E3933FF0000
      000039A8DBFF00000000000000000000000000000000000000000000000039A8
      DBFF000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD6C
      39FF88538EFF6750DDFF6D5CEDFF6D5CEDFFABA1F5FF0000000000000000BD6C
      39FFBD6C39FFE2C0AAFF00000000000000000000000000000000000000000000
      000000000000000000003D8B45F9BCB270FFE8A05CFFBB8035FE000000110000
      0000502C1099E69E5FFFE1924CFF22120566171513FF7B7167FF9F9489FFA094
      88FF46423FFF0000000047413CFF7A7066FFABA197FF90857AFF47413BFF0000
      00000000000000000000000000000000000000000000000000000000000039A8
      DBFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000D6A586FFC277
      48FFCA8A5FFFABA1F5FF6D5CEDFF6D5CEDFF6D5CEDFFABA1F5FF00000000D39E
      7AFFC27748FFCC9066FF00000000000000000000000000000000000000000000
      0000000000001730198A6FB678FFB6C590FFE69B53FFE8A463FF3F260F880000
      00003F250E88E69F5FFFE59856FF2F1A08771D1A18FF786F65FFA0978CFF9A8F
      84FF43403DFF00000000534E4AFF756B62FFAEA59BFF90867BFF504A44FF0000
      00000000000000000000000000000000000000000000000000000000000039A8
      DBFF0000000000000000433E37FF5A534CFF4D4741FF2F2B27FF000000073A36
      34FF3C3833FF302C28FF070505FF000000000000000000000000C9885CFFC072
      41FFC27647FF00000000ABA1F5FF6D5CEDFF6D5CEDFF6D5CEDFFABA1F5FFC988
      5CFFC27748FFC9885CFF00000000000000000000000000000000000000000000
      00001C381F8D80C588FFB0DCB6FF73B87AFF888B42FAE49648FFE8A35FFFE28B
      3AFFE69E5AFFE9A96DFFE49954FF231407664D4A47FFA69A8EFFBBB0A5FF9C91
      86FF6D6A67FF00000000827A74FFA6998EFFC2B9AFFFA3978CFF76706CFF0000
      0000000000000000000000000000000000000000000039A8DBFF39A8DBFF0000
      000039A8DBFF39A8DBFF554E47FFA89D94FF8F8579FF4F4944FF000000075E58
      52FFADA195FF8C8275FF181614FF00000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF00000000ABA1F5FF6D5CEDFF6D5CEDFF6D5CEDFF8853
      8EFFBD6C39FFE2C0AAFF00000000000000004F341E96E29253FFD5854FFF0101
      0017264A2A9F88CB90FF83C68BFF819A4FFF00000003467539E5301F0E77794C
      21BB794B1FBB643D19AA23150866000000000F0F0FFF110F0EFF1E1B19FF1B19
      17FF585654FFC6C6C5FF363330FF36312DFF4C4640FF443F39FF5E5751FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000078716CFFB9AEA3FFB2A69BFF817972FF000000007671
      6CFFB9AEA2FFB3A699FF484542FF00000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF0000000000000000ABA1F5FF6D5CEDFF6D5CEDFF6041
      C8FF88538EFFE2C0AAFF0000000000000000000000000705032FD8925FFF0D08
      054100000000274C2CA189A85CFF875028C80000000000000000000000000000
      0000000000000000000000000000000000001A1816FF756B61FF786C62FF8476
      6BFF695E54FF4C453DFF7B7065FF7D7267FF7D7369FFA99D90FFA3978BFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000005A534DFF47423BFF3D3733FF302C29FFC5C5C4FF3E3C
      3AFF1A1816FF131110FF0F0F0FFF00000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF000000000000000000000000ABA1F5FF6D5CEDFF6041
      C8FF6041C8FF9B80C8FF00000000000000000000000000000000452F1C8CD697
      66FF3C26188A0C07043FE29457FF5B361CA30000000000000000000000000000
      00000000000000000000000000000000000053514FFF7B6F64FF897464FF937D
      6BFF958370FFA19181FF8E7D6EFF867668FF897C72FFA89B90FFC8BFB7FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B1A599FF9D9286FF83766AFF897A6DFF544A40FF8070
      64FF8A796DFF86786CFF242220FF00000000000000000000000000000000D4A1
      7DFFBD6D3AFFBE6E3BFF000000000000000000000000000000008D5D9BFF6042
      C9FF6347D0FF6D5CEDFFABA1F5FF00000000000000000000000000000000E29C
      60FF7C5235C201000014E59455FF2E1C0F740000000000000000000000000000
      000000000000000000000000000000000000BBBAB9FF6A635DFF7A6D61FF8574
      66FF5B544CFFC0BCB8FF544D46FF887B70FF9B8E82FF897C71FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000004844409E908376FF8A7B6EFF6D6257FEB5ABA0FF796A
      5CFE877362FF6C6056FE8E8D8BFF000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000ABA1
      F5FF6D5CEDFF6D5CEDFF6D5CEDFF000000000000000000000000000000000000
      0000DA9963FF593C25A2EBA265FF0B07043A0000000000000000000000000000
      00000000000000000000000000000000000000000000E2E1E0FFBAB3ADFFA094
      88FF756B62FF00000000756D64FFACA297FFCFC9C4FFE5E3E1FF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000505052ACFCAC5FFAFA59BFE6D655DFF09080839766C
      62FFA4998FFEC7C4C1FF08080834000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000ABA1F5FF6D5CEDFFABA1F5FF000000000000000000000000000000000000
      00006A4B31ADE2B089FFE8AB76FF25190F680000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B8B4B0FF9489
      7DFFAC9F92FF00000000A5998DFFB3A79CFFDFDBD7FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000002929286DC2B9B0FFA69A8EFF00000007A598
      8CFFA9A29CFF0505052C00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E0A771FFE2A66CFFE09F68FFB57F54EA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000002418
      0F66E69A5FFFE5975CFFE29459FFE39358FF080503330000000051331D99E28D
      51FFE28C4FFFE28A4EFFE28A4EFF51311C990000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000039A8DBFF39A8DBFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B58484FFB584
      84FFB58484FFB58484FFB58484FFB58484FFB58484FFB58484FFB58484FFB584
      84FFB58484FFB58484FFB58484FF000000000000000000000000000000000000
      001131211577E8A571FFD98F5DFE603E28A9000000100000000003020122301F
      1277E59A66FFEAAD83FFE4945CFF0F0905440000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000039A8
      DBFF39A8DBFF39A8DBFF39A8DBFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59CFFFFEF
      D6FFF7E7C6FFF7DEBDFFF7DEB5FFF7D6ADFFF7D6A5FFF7CE9CFFF7CE94FFF7CE
      94FFF7CE94FFF7D69CFFB58484FF000000000000000000000000000000000000
      00000000000024190F66E5A573FFDD9866FF0604022D00000000000000000000
      0000E39055FFE9A778FF905A34CC000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000039A8DBFF39A8
      DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C6A59CFFFFEF
      D6FF848484FF848484FF848484FF848484FF848484FF848484FF848484FF8484
      84FF848484FFF7CE9CFFB58484FF000000000000000000000000000000000000
      00000000000000000000412D1D88E1A577FFCA8859F5784E34BD774E32BD764C
      31BDDF9A6AFFE9A777FF794C2DBB000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000039A8DBFF39A8DBFF00000000000000003E3933FF48423CFF47423CFF3834
      2FFF282421FF00000000353232FF262320FF22201DFF181614FF030202FF0000
      000039A8DBFF39A8DBFF00000000000000000000000000000000C6ADA5FFFFF7
      E7FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF
      84FF84FF84FFF7CE9CFFB58484FF000000000000000000000000000000000000
      0000000000000000000000000011B57F52E3E3AA80FF65442DAD000000030000
      0000E5975BFFE9A674FF51341F99000000003E3933FF48423CFF47423CFF3834
      2FFF282421FF00000000353232FF262320FF22201DFF181614FF030202FF0000
      000039A8DBFF39A8DBFF000000000000000047413BFF90857AFFABA197FF7A70
      66FF47413CFF0000000046423FFFA09488FF9F9489FF7B7167FF171513FF0000
      000039A8DBFF39A8DBFF00000000000000000000000000000000C6ADA5FFFFF7
      E7FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF
      84FF84FF84FFF7CE94FFB58484FF000000000000000000000000000000000000
      000000000000000000000000000009060433E5A470FFD59562FA17100A533121
      1577E9A671FFE8A570FF312013770000000047413BFF90857AFFABA197FF7A70
      66FF47413CFF0000000046423FFFA09488FF9F9489FF7B7167FF171513FF0000
      000039A8DBFF39A8DBFF0000000000000000504A44FF90867BFFAEA59BFF756B
      62FF534E4AFF0000000043403DFF9A8F84FFA0978CFF786F65FF1D1A18FF0000
      000039A8DBFF39A8DBFF00000000000000000000000000000000C6ADA5FFFFFF
      F7FF848484FF848484FF848484FF848484FF848484FF848484FF848484FF8484
      84FF848484FFF7CE9CFFB58484FF000000000000000000000000000000000000
      000033753AE537853DFF2F7835FF286E2DFF828246FBE4AA7AFF956945D13323
      1779EAAA77FFE9A874FF3121147700000000504A44FF90867BFFAEA59BFF756B
      62FF534E4AFF0000000043403DFF9A8F84FFA0978CFF786F65FF1D1A18FF0000
      000039A8DBFF39A8DBFF000000000000000076706CFFA3978CFFC2B9AFFFA699
      8EFF827A74FF000000006D6A67FF9C9186FFBBB0A5FFA69A8EFF4D4A47FF0000
      000039A8DBFF39A8DBFF00000000000000000000000000000000DEC6B5FFFFFF
      FFFF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF
      84FF84FF84FF84FF84FFB58484FF000000000000000000000000000000000000
      0000000000003D8B45F99BD2A2FF95D09DFF26682BF8AA7A51DBD69967F97754
      37BAEDB487FFE9A670FF100A06440000000076706CFFA3978CFFC2B9AFFFA699
      8EFF827A74FF000000006D6A67FF9C9186FFBBB0A5FFA69A8EFF4D4A47FF0000
      000039A8DBFF39A8DBFF00000000000000005E5751FF443F39FF4C4640FF3631
      2DFF363330FFC6C6C5FF585654FF1B1917FF1E1B19FF110F0EFF0F0F0FFF0000
      000039A8DBFF39A8DBFF00000000000000000000000000000000DEC6B5FFFFFF
      FFFF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF
      84FF84FF84FF84FF84FFB58484FF000000000000000000000000000000000000
      00001730198A6FB678FFA6D8ADFF9ED3A5FF2F7635FC04020122DBA06AFADD9E
      6BFCEEBC95FFEAA973FF100B0744000000005E5751FF443F39FF4C4640FF3631
      2DFF363330FFC6C6C5FF585654FF1B1917FF1E1B19FF110F0EFF0F0F0FFF0000
      000039A8DBFF39A8DBFF0000000000000000A3978BFFA99D90FF7D7369FF7D72
      67FF7B7065FF4C453DFF695E54FF84766BFF786C62FF756B61FF1A1816FF0000
      000039A8DBFF39A8DBFF00000000000000000000000000000000DEC6B5FFFFFF
      FFFF848484FF848484FF848484FF848484FF848484FF848484FF848484FF8484
      84FF848484FFF7DEB5FFB58484FF00000000A45619DDD96E1CFF894310CC5264
      32CA80C588FFB0DCB6FF73B87AFF3B8542F535813BFBBB8A5FE7D19A68F4EABB
      92FFE9BA93FFE7B489FFB88357E800000000A3978BFFA99D90FF7D7369FF7D72
      67FF7B7065FF4C453DFF695E54FF84766BFF786C62FF756B61FF1A1816FF0000
      000039A8DBFF39A8DBFF0000000000000000C8BFB7FFA89B90FF897C72FF8676
      68FF8E7D6EFFA19181FF958370FF937D6BFF897464FF7B6F64FF53514FFF0000
      000039A8DBFF39A8DBFF00000000000000000000000000000000DEBDB5FFFFFF
      FFFF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF
      84FF84FF84FFF7DEB5FFB58484FF00000000E49553FFBE661FEE2F1806777E93
      49F288CB90FF83C68BFF35582DBF00000003327138E14232228A4A3625919F74
      50D4B4855AE2B48257E24C36259400000000C8BFB7FFA89B90FF897C72FF8676
      68FF8E7D6EFFA19181FF958370FF937D6BFF897464FF7B6F64FF53514FFF0000
      000039A8DBFF39A8DBFF000000000000000000000000897C71FF9B8E82FF887B
      70FF544D46FFC0BCB8FF5B544CFF857466FF7A6D61FF6A635DFFBBBAB9FF0000
      000039A8DBFF39A8DBFF00000000000000000000000000000000DEC6B5FFFFFF
      FFFF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF
      84FF84FF84FFC6BDADFFB58484FF00000000E18E40FFC26C27EE08040133180C
      035592AA5BFF7F994BF900000003000000000000000000000000000000000000
      00000000000000000000000000000000000000000000897C71FF9B8E82FF887B
      70FF544D46FFC0BCB8FF5B544CFF857466FF7A6D61FF6A635DFFBBBAB9FF0000
      000039A8DBFF39A8DBFF00000000000000000000000000000000CFC9C4FFACA2
      97FF756D64FF00000000756B62FFA09488FFBAB3ADFF00000000000000000000
      000039A8DBFF39A8DBFF00000000000000000000000000000000E7C6B5FFFFFF
      FFFF848484FF848484FF848484FF848484FF848484FFFFF7EFFFF7E7D6FFC6A5
      94FFB5948CFFB58C84FFB58484FF00000000180F0655A76326DDE18C3FFF8D4E
      1ACCE7A36AFFDE8138FF0F070144000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E5E3E1FFCFC9C4FFACA2
      97FF756D64FF00000000756B62FFA09488FFBAB3ADFFE2E1E0FF000000000000
      0000000000000000000000000000000000000000000000000000AAA7A4DFB3A7
      9CFFA5998DFF00000000AC9F92FF94897DFFB8B4B0FF0000000039A8DBFF39A8
      DBFF39A8DBFF39A8DBFF39A8DBFF39A8DBFF0000000000000000E7C6B5FFFFFF
      FFFF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FFE7CECEFFBD8C
      71FFEFB571FFEFA548FFC68469FF0000000003020122794B1FBB301D0C77301C
      0A778E4F1CCCE49857FF3D200988000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DFDBD7FFB3A7
      9CFFA5998DFF00000000AC9F92FF94897DFFB8B4B0FF00000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000039A8
      DBFF39A8DBFF39A8DBFF39A8DBFF000000000000000000000000EFCEBDFFFFFF
      FFFF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FF84FFE7D6CEFFC694
      79FFFFC671FFCE9471FF000000000000000003020122E6984CFFA96A2FDD0000
      00113F250E88E69F5EFF774115BB000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000039A8DBFF39A8DBFF00000000000000000000000000000000E7C6B5FFFFF7
      F7FFFFF7EFFFFFF7EFFFFFF7EFFFFFF7EFFFFFF7EFFFFFF7EFFFE7CECEFFC694
      79FFCE9C84FF0000000000000000000000000000000019100755AA6F32DDE393
      42FFE69A50FFE28E41FF3F250D88000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000E7C6B5FFEFCE
      B5FFEFCEB5FFEFCEB5FFEFCEB5FFE7C6B5FFE7C6B5FFEFCEB5FFDEBDB5FFBD84
      79FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DFCFC4FF87481EFF87481EFFDFCF
      C4FF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E
      8DFF8C8E8DFF8C8E8DFF8C8E8DFFACAEADFF0000000000000000000000000000
      000000000000000000000000000000000000000000000000011601040739020D
      15620418298805233AA411588EFC0A4A7CED0000000000000000000000001761
      ADFF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000087481EFFB59578FFA48D76FF8748
      1EFFDFCFC4FFFDFDFDFFFDFDFDFFFCFCFCFFFDFDFDFFFDFDFDFFFCFCFCFFFDFD
      FDFFFCFCFCFFF9F9F9FDE3E3E3F38C8E8DFF999999FF6F6F6FFF525252FF4F4F
      4FFF4D4D4DFF4A4A4AFF484848FF454545FF434343FF23659DFF3072A8FF3B7A
      AFFF4584B5FF4C8ABAFF3C7CADFF094878EA00000000000000001761ADFF3299
      FFFF1761ADFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E2C0AAFFE2C0AAFF00000000000000000000000000000000E2C0AAFFE2C0
      AAFF0000000000000000000000000000000087481EFFCCBBADFFB59578FFA48D
      76FF87481EFFDFCFC4FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFCFCFCFFFDFD
      FDFFFDFDFDFFFDFDFDFFF9F9F9FD8C8E8DFF0000000000000000565656FFA2A2
      A2FFA2A2A2FFA3A3A3FFA4A4A4FFA4A4A4FFA5A5A5FF2D6DA5FF76ABD2FF76AB
      D3FF71A7D1FF67A0CDFF3E7DAEFF0B4A7AEA000000001761ADFF369CFFFF349A
      FFFF3299FFFF1761ADFF000000000000000013831EFFA5DBA9FF000000000000
      000000000000000000000000000000000000000000000000000000000000C886
      59FFBE6F3DFFC58051FF00000000000000000000000000000000CE946CFFBE6E
      3CFFC17544FF000000000000000000000000DFCFC4FF87481EFFCCBBADFFB595
      78FFA48D76FF87481EFFE3E3E3FFE1E1E1FFE0E0E0FFDEDEDEFFDDDDDDFFDEDE
      DEFFDDDDDDFFFCFCFCFFFCFCFCFF8C8E8DFF00000000000000005A5A5AFFA1A1
      A1FF3A713EFFA0A1A1FFA3A3A3FFA3A3A3FFA4A4A4FF3472AAFF7BAFD4FF599A
      C9FF5295C7FF5696C8FF3F80AEFF0F4C7DEA1761ADFF1761ADFF1761ADFF3EA2
      FFFF1761ADFF1761ADFF1761ADFF0000000013831EFF13831EFF000000000000
      000000000000000000000000000000000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF0000000000000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF000000000000000000000000DFCFC4FF87481EFFCCBB
      ADFF87481EFF858A88FF858A88FFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFF8C8E8DFF00000000000000005E5E5EFFA0A0
      A0FF3B743FFF346F37FFA2A2A2FFA2A2A2FFA3A3A3FF3B77B0FF82B3D7FF609F
      CCFF589AC9FF5C9BCAFF4181AFFF144F7FEA00000000000000001761ADFF4CAD
      FFFF1761ADFF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF0000000000000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF00000000000000000000000000000001A29187DE8748
      1EFF858A88FFE2E3E5FFB7B8B9FF858A88FF858A88FF858A88FFB4B4B4FFD4D4
      D4FFD2D2D2FFF9F9F9FFFDFDFDFF8C8E8DFF35823CFF327C39FF2F7735FF2C73
      32FF47914EFF448F4AFF37713BFFA1A1A1FFA2A2A2FF437CB4FF88B7D9FF65A3
      CFFF5F9ECCFF619FCCFF4383B1FF195383EA00000000000000001761ADFF5DB9
      FFFF1761ADFF00000000000000000000000013831EFFAFE0B2FF9ED7A3FF8ECD
      93FF00000000000000000000000000000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF0000000000000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF000000000000000000000000000000018C8E8DFFFBFB
      FBFF858A88FFB7B8B9FFE2E3E5FFE2E3E5FFE2E3E5FFE2E3E5FF858A88FFFAFA
      FAFFFAFAFAFFF9F9F9FFFDFDFDFF8C8E8DFF398740FF89CB92FF84C88DFF80C6
      88FF79C383FF75C17DFF458F4BFF39723DFFA1A1A1FF4A84BAFF8DBBDBFF6CA8
      D1FF64A6D1FF5DB4DFFF4585B1FF1E5688EA00000000000000001761ADFF6EC6
      FFFF1761ADFF00000000000000000000000013831EFF13831EFF13831EFF1383
      1EFF000000000000000000000000000000000000000000000000D6A586FFC277
      48FFCA8A5FFF000000000000000000000000000000000000000000000000D39E
      7AFFC27748FFCC9066FF000000000000000000000000000000018C8E8DFFFCFC
      FCFFC8C8C8FF858A88FFE2E3E5FF858A88FF858A88FFE2E3E5FF858A88FFCACA
      CAFFC8C8C8FFF8F8F8FFFDFDFDFF8C8E8DFF3C8B44FF8FCE99FF7BC687FF76C3
      81FF71C07AFF72C07AFF77C281FF47904DFF527D55FF5289BFFF94BFDDFF73AD
      D4FF61B8E1FF49D4FFFF408BB8FF245B8BEA00000000000000001761ADFF80D4
      FFFF1761ADFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000C9885CFFC072
      41FFC27647FF000000000000000000000000000000000000000000000000C988
      5CFFC27748FFC9885CFF000000000000000000000000000000018C8E8DFFFCFC
      FCFFFCFCFCFF858A88FFE2E3E5FF858A88FFE2E3E5FF858A88FFFCFCFCFFF8F8
      F8FFF6F6F6FFF5F5F5FFFDFDFDFF8C8E8DFF3F9048FF94D29FFF91D09AFF8DCD
      96FF89CB92FF84C88DFF4F9856FF3F7A44FF9F9F9FFF588EC4FF98C3E0FF7AB3
      D7FF72AFD6FF5CC4EDFF4988B3FF2A5F90EA00000000000000001761ADFF91E0
      FFFF1761ADFF00000000000000000000000013831EFFA7DDACFF94D099FF81C5
      86FF6BB871FF57AD5FFF0000000000000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF0000000000000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF000000000000000000000000000000018C8E8DFFFDFD
      FDFFBCBCBCFF858A88FFE2E3E5FFE2E3E5FF858A88FFBCBCBCFFBCBCBCFFBCBC
      BCFFBCBCBCFFF1F1F1FFFDFDFDFF8C8E8DFF42944BFF409149FF3D8D46FF3B89
      43FF5BA463FF58A05FFF438349FF9E9E9EFF9E9E9EFF5E92C9FF9EC7E2FF83B8
      DAFF7BB4D7FF7CB3D7FF4D89B4FF306494EA00000000000000001761ADFF9FEA
      FFFF1761ADFF00000000000000000000000013831EFF13831EFF13831EFF1383
      1EFF13831EFF13831EFF0000000000000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF0000000000000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF000000000000000000000000000000018C8E8DFFFCFC
      FCFFFCFCFCFFFCFCFCFF858A88FF858A88FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFF4F4F4FFFCFCFCFF8C8E8DFF0000000000000000757575FF9A9A
      9AFF3B8A43FF478A4DFF9C9C9CFF9D9D9DFF9D9D9DFF6496CCFFA2CBE3FF89BD
      DCFF83B9DAFF84B9DAFF4F8BB5FF376999EA00000000000000001761ADFFA9F3
      FFFF1761ADFF0000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF0000000000000000000000000000000000000000BD6C
      39FFBD6C39FFE2C0AAFF000000000000000000000000000000018C8E8DFFFCFC
      FCFFACAEADFFACAEADFFACAEADFFACAEADFFACAEADFFACAEADFFACAEADFFACAE
      ADFFACAEADFFFCFCFCFFFDFDFDFF8C8E8DFF0000000000000000787878FF9999
      99FF509157FF999A99FF9B9B9BFF9C9C9CFF9C9C9CFF6A9AD0FFA7CEE5FF8FC1
      DFFF89BDDCFF8BBDDCFF518DB6FF3E6E9EEA00000000000000001761ADFFABF4
      FFFF1761ADFF00000000000000000000000013831EFFB0E1B4FFA2D8A6FF93D0
      99FF85C78BFF75BF7BFF67B66EFF59AE61FF000000000000000000000000D4A1
      7DFFBD6D3AFFBE6E3BFF00000000000000000000000000000000C68153FFBF70
      3FFFCC8D64FF00000000000000000000000000000000000000018C8E8DFFFDFD
      FDFFF9F9F9FFFAFAFAFFF9F9F9FFF9F9F9FFF6F6F6FFF4F4F4FFEFEFEFFFFDFD
      FDFFF8F8F8FFF7F7F7FF8C8E8DFFACAEADFF00000000000000007B7B7BFF9999
      99FF999999FF9A9A9AFF9A9A9AFF9B9B9BFF9B9B9BFF6D9DD3FFAAD1E7FFABD1
      E7FF98C7E1FF91C2DEFF548FB7FF4373A2EA00000000000000001761ADFFABF4
      FFFF1761ADFF00000000000000000000000013831EFF13831EFF13831EFF1383
      1EFF13831EFF13831EFF13831EFF13831EFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008C8E8DFFF8F8
      F8FDF6F6F6FFF7F7F7FFF8F8F8FFF7F7F7FFF5F5F5FFF3F3F3FFEEEEEEFFFCFC
      FCFFECECECFF8C8E8DFFACAEADFF000000020000000000000000808080FF7C7C
      7CFF7A7A7AFF787878FF757575FF737373FF707070FF6F9ED4FF6D9ED6FF87B2
      DCFFABD3E8FFA9D0E6FF5690B8FF4A77A6EA00000000000000001761ADFF1761
      ADFF1761ADFF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000008C8E8DFFDDDD
      DDF0F8F8F8FDFDFDFDFFFDFDFDFFFCFCFCFFFDFDFDFFFCFCFCFFFDFDFDFFFAFA
      FAFF8C8E8DFFACAEADFF00000002000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005175
      9DDB6B9CD4FF85B1DAFF5891B9FF4F7CAAEA0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000ACAEADFF8C8E
      8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E
      8DFFACAEADFF0000000200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000001F2C3C896495CAFB5784B4EE0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000412714A2000000010000
      00000000000000000005110A05535C391FBE8A562DE99E6031FA85532DE4492F
      19AA030100240000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000031B
      2069041B20690001011800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A46332FF362111930000
      0000110A055491582FEEA4734BF3CEA684FFD8B697FFDBB999FFD3AC8AFFBD90
      69FC9A5F32F6140C065B00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000015
      195D306A72B2074C5AAE000000000000000000000000000000003A3A3AFF3333
      33FF2D2D2DFF272727FF222222FF1C1C1CFF171717FF121212FF0D0D0DFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A36535FEAB7C53F77246
      24D4A26E46F4E3CAB4FFECDAC9FFE7D1BCFFE3C9B0FFDEBEA0FFD2AB88FFCEA5
      82FFD3AE8EFF995E32F50402012A000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000002
      03223FBED5FC41C3D6FE08282F7F000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A46436FDF1E4D8FFD2B0
      93FEF4E9E0FFF3E8DDFFEDDCCCFFD0AB8DFEA26D44F59F6232FBA66737FFA467
      37FEA96B3BFFB0764AFF482D18A8000000000000000000000000000000000000
      0000000000000000000000A0C4FF000000000000000000000000000000000000
      00000CA8C9FE5BD8E7FE1D91A8EF000000000000000000000000484848FF4242
      42FF3C3C3CFF363636FF303030FF2A2A2AFF242424FF1E1E1EFF191919FF1414
      14FF0F0F0FFF0A0A0AFF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A36335FDF6EEE6FFF5EC
      E3FFF5EDE4FFE6D2C1FFA26E45F5684022CA07040236000000001C10086A925B
      32EDB67A4DFFA56838FE9F6233FA000000000000000000000000000000000000
      00000000000000A0C4FF00A0C4FF000000000000000000000000000000000001
      011A00ACC8FF86E5F0FE0FA2C2FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A06233FCF6EEE6FFEBD7
      C4FFEAD9C9FFA26332FE1C10086A0000000000000000000000000000000B8A53
      2BE93D2D208D6A482EC2A46332FF000000050000000000000000000000000000
      001100A0C4FF74EDFBFF00A0C4FF000000000000000000050630000F12500062
      79C96BE6F5FF74E2EFFF17A3C1FF000000000000000000000000000000009E68
      3DD400000000000000003C3C3CF8363636F8303030F82A2A2AF8242424F81F1F
      1FF81B1B1BF8161616F800000000000000000000000000000000806B5CFF7A5F
      4AFF806047FF816047FF816047FF816047FF816047FF836046FF816146FF8160
      47FF7A5F4AFF806B5CFF000000000000000000000000A06132FCF5EDE5FFF6ED
      E5FFF5ECE4FFD3B39AFD804F2AE0000000100000000000000000000000000000
      00000201011A0A070539A06132FC0000000C00000000000000000000001100A0
      C4FF74EDFBFF74EDFBFF00A0C4FF00A0C4FF00A0C4FF00A0C4FF00A9C4FF6CE1
      EEFF0DC9DFFF67E4F2FF189BB5F80000000000000000050301299C693DD69F6D
      43D40000000000000000303030D42C2C2CD4282828D4232323D41F1F1FD41B1B
      1BD4181818D4131313D4000000000000000000000000000000007A5F4AFFE6CC
      B4FFD1B69CFFD2B59AFFD1B398FFD2B295FFCBAB8EFFCBA98CFFC8A689FFC6A3
      89FFDCBDA4FF7C5F4AFF0000000000000000000000009C5F30F9A26332FEA263
      32FEA26132FDA06132FC9E6131FB55341AB90201001D01000018010000180100
      001801000018010000180101001C00000000000000000000001100A0C4FF74ED
      FBFF02C3DAFF74EDFBFF67EAF9FF67EAF9FF67EAF9FF67EAF9FF03DDF7FF08C8
      DFFF05C2D8FF6DDCEBFF1795AFF400000000050301298F633FCBE9B88FFFECB9
      91FFE3A570FF0000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007A5F4AFFEED4
      BCFFEDD2B8FFEFD2B7FFEDCFB4FFEFCFB2FFEECEB1FFF1CFB2FFF0CEB1FFEFCC
      B2FFEBCCB3FF7C5F4AFF0000000000000000000000000000000D000000000000
      000000000000000000000000000000000000402714A0A46332FFA36C41F8A46D
      46F7A46D46F7A46E46F7A46332FF000000080000001100A0C4FF77EDFBFF30E2
      F8FF2ADFF4FF02C0D6FF02C0D6FF02C0D6FF1BD2E8FF1BD2E8FF1BD2E8FF09C8
      DFFF68E5F3FF19A1BAF80C6476CB0000000000000000050301299F734CD5A076
      53D40000000000000000505050F84A4A4AF8444444F83E3E3EF83A3A3AF83434
      34F82E2E2EF8282828F800000000000000000000000000000000806B5CFF7A5F
      4AFF806047FF816047FF816047FF826247FF816146FF836046FF836046FF8460
      48FF7C5F4AFF806B5CFF000000000000000000000000A06132FC2B1D127E0504
      032B0000001300000002000000000000000000000004613C20C4C8A589FAF6EE
      E7FFF2E6DBFFF6EEE6FFA06436FB0000000900A0C4FFADF3FBFF2DE0F6FF30E2
      F8FF30E2F7FF30E2F7FF2DE0F5FF27DBF1FF1BD2E8FF1BD2E8FF1BD2E8FF34D9
      ECFF3ECDE1FF0D6476CA0000000A00000000000000000000000000000000A379
      5BD40000000000000000404040D73D3D3DD7393939D7343434D72F2F2FD72B2B
      2BD7272727D7232323D700000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A26333FEA16436FB4A36
      269D86522CE603010023000000000000000000000000160D0760A46433FFE9D7
      C7FFEBD8C6FFF5ECE3FFA06436FA0000000A0000000A00A0C4FFADF3FBFF2DE0
      F6FF30E2F7FF27DBF1FF2DE0F5FF27DBF1FF14CDE3FF34D9ECFF67E7F6FF3FCC
      E1FE0E829AE40004043200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000965D32F3AB6E3FFFA76A
      3AFE9A6034F522150A75010000190B0703456B4323CDAB7E58F5EBDBCDFFF5EB
      E2FFF6EEE6FFF6EEE6FFA16436FA0000000B000000000000001100A0C4FFADF3
      FBFF2FE1F6FF1EE3FAFF71ECFAFF6DEBFAFF6CE8F7FF64DAE9F80D6F82D40D71
      86D50001011C0000000000000000000000000000000000000000747474FF7272
      72FF6E6E6EFF6A6A6AFF666666FF616161FF5C5C5CFF575757FF515151FF4C4C
      4CFF464646FF404040FF00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003D26149BBC8E65FDC598
      70FFA8693AFFA46433FFA36638FCA67853F3D7B99FFEF1E4D8FFF2E6DBFFF3E8
      DDFFCAA586FDEAD8C8FF9F6436F90000000D00000000000000000000001100A0
      C4FFADF3FBFF23E4FBFF00A0C4FF00A0C4FF0F849CE70C697BCF0A414CA40000
      000B000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000004020129995F33F5D3AD
      8CFFDCBD9DFFDDBEA1FFE5CBB4FFE9D3BFFFEEDDCCFFF0E2D5FFE7D2BFFFA16C
      43F55D391EC0A0693DF7A06233FC0000000E0000000000000000000000000000
      001100A0C4FFADF3FBFF00A0C4FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000F0905509A5F
      32F6B88A62FAD3B08FFFDFC2A8FFDEC1A8FFD4B193FFA97B55F4925A2FF0130B
      055800000000190F0866A46332FF0000000F0000000000000000000000000000
      00000000011400A0C4FF00A0C4FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000201
      001D4028169F7E4E2BDE995E31F687532CE55A381EBC110A0553000000050000
      0000000000000000000024160B79000000100000000000000000000000000000
      0000000000000000000000A0C4FF000000000000000000000000000000000000
      000000000000000000000000000000000000DFCFC4FF87481EFF87481EFFDFCF
      C4FF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E
      8DFF8C8E8DFF8C8E8DFF8C8E8DFFACAEADFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000F374FF7265B83FB4584B7FB24506BC10001032200000000000000000000
      000000000000000000000000000000000000000000002D2F2E94858A88FF858A
      88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF2E2F2F940000000087481EFFB59578FFA48D76FF8748
      1EFFDFCFC4FFFDFDFDFFFDFDFDFFFCFCFCFFFDFDFDFFFDFDFDFFFCFCFCFFFDFD
      FDFFFCFCFCFFF9F9F9FDE3E3E3F38C8E8DFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000029617FFB94C7F9FF91C9F9FF3F85C9FF18599AF30104062F000000000000
      00000000000000000000000000000000000000000000838886FEF8F8F8FFF8F8
      F8FFF8F8F8FFF8F8F8FFF8F8F8FFF8F8F8FF000000FFF8F8F8FF000000FFF8F8
      F8FFF8F8F8FFF8F8F8FF868B89FE0000000087481EFFCCBBADFFB59578FFA48D
      76FF87481EFFDFCFC4FFFDFDFDFFFDFDFDFFFDFDFDFFFDFDFDFFFCFCFCFFFDFD
      FDFFFDFDFDFFFDFDFDFFF9F9F9FD8C8E8DFF000000003C3732FF37322EFF312D
      29FF2A2723FF25221FFF1E1B19FF0000001A0B0A09DB090807FF050504FF0202
      01FF000000FF000000FF00000000000000000000000000000000000000000000
      00004189AAFFE0F2FFFF529AD8FF1878BEFF4798C5FF3E81B6FF000000000000
      00000000000000000000000000000000000000000000858A88FFF9F9F9FFDBC3
      ADFFDCC4AEFFDDC5AFFFDEC6B0FFDFC7B1FFE0C8B2FF000000FFF7F7F7FFF9F9
      F9FFF8F8F8FFF7F7F7FF888D8BFF00000000DFCFC4FF87481EFFCCBBADFFB595
      78FFA48D76FF87481EFFE3E3E3FFE1E1E1FFE0E0E0FFDEDEDEFFDDDDDDFFDEDE
      DEFFDDDDDDFFFCFCFCFFFCFCFCFF8C8E8DFF00000000443F39FF85786EFFC3B8
      AEFF7A7066FF7D7369FF34302BFF0000000F1B1817D495897BFFBAAEA2FF7A70
      66FF7D7369FF000000FF00000000000000003C3732FF37322EFF312D29FF2A27
      23FF97B5C9FF78B6D5FF90B7D1FF53C9E4FF59DFF5FF76D0EDFF4D9BDBFF0000
      00FF000000FF00000000000000000000000000000000858A88FFF9F9F9FFDBC3
      ADFFB09882FFB09882FFB09882FFB09882FFE0C8B2FF000000FFF7F7F7FFF8F8
      F8FFF7F7F7FFF7F7F7FF888D8BFF0000000000000000DFCFC4FF87481EFFCCBB
      ADFF87481EFF858A88FF858A88FFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFBFBFBFFFCFCFCFF8C8E8DFF000000004B453FFF83766DFFCCC3
      BAFF766D63FF796F65FF2B2724F900000001181714EE95897BFFC2B8ADFF766D
      63FF7A7066FF040303FF0000000000000000443F39FF85786EFFC3B8AEFF7A70
      66FF7D7369FFA4C6D7FF73B8D6FFC2F6FDFF61DFF7FF5BE2F8FF77D3F0FF4899
      DCFF000000FF00000000000000000000000000000000858A88FFFAFAFAFFDBC3
      ADFFDCC4AEFFDDC5AFFFDEC6B0FFDFC7B1FFE0C8B2FF000000FFF5F5F5FFF6F6
      F6FFF6F6F6FFF7F7F7FF888D8BFF000000000000000000000001A29187DE8748
      1EFF858A88FFE2E3E5FFB7B8B9FF858A88FF858A88FF858A88FFB4B4B4FFD4D4
      D4FFD2D2D2FFF9F9F9FFFDFDFDFF8C8E8DFF000000004F4842FC83766DFFCCC3
      BAFF776E64FF6F665DFF25221FD500000000181715D585786EFFC2B8ADFF766D
      63FF796F65FF080706FC00000000000000004B453FFF83766DFFCCC3BAFF766D
      63FF796F65FF2B2724F989AEBFFF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF4A9ADEFF00000000000000000000000000000000858A88FFFBFBFBFFDBC3
      ADFFDCC4AEFFDDC5AFFFDEC6B0FFDEC6B0FF000000FFE0C8B2FF000000FFE1C9
      B3FFF4F4F4FFF7F7F7FF888D8BFF0000000000000000000000018C8E8DFFFBFB
      FBFF858A88FFB7B8B9FFE2E3E5FFE2E3E5FFE2E3E5FFE2E3E5FF858A88FFFAFA
      FAFFFAFAFAFFF9F9F9FFFDFDFDFF8C8E8DFF00000000332F2BC39F9286FFCCC3
      BAFFC0B4AAFFA6988BFF1A1816A80000000012110FA8908477FFC2B8ADFFC0B4
      AAFFA89B8EFF090807C300000000000000004F4842FC83766DFFCCC3BAFF776E
      64FF6F665DFF25221FD50000000095BDCAFF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF4EA0E0FF000000000000000000000000858A88FFFBFBFBFFDAC2
      ACFFAF9781FFAF9781FFAF9781FFAF9781FFAF9781FFAF9781FFAF9781FFE0C8
      B2FFF2F2F2FFF7F7F7FF888D8BFF0000000000000000000000018C8E8DFFFCFC
      FCFFC8C8C8FF858A88FFE2E3E5FF858A88FF858A88FFE2E3E5FF858A88FFCACA
      CAFFC8C8C8FFF8F8F8FFFDFDFDFF8C8E8DFF00000005564F48F9403B36FF564F
      48FF3B3631FF312D29FF1B1916E5000000300E0D0CB5181614FF23201EFF1715
      13FF0D0C0BFF000000EE0000000200000000332F2BC39F9286FFCCC3BAFFC0B4
      AAFFA6988BFF1A1816A80000000012110FA8B5DEEBFF7BD4EEFFC4F6FDFF6ADD
      F6FF6BCAEDFF61A3D7FF6199C9FF0103042600000000858A88FFFCFCFCFFDAC2
      ACFFDBC3ADFFDCC4AEFFDDC5AFFFDDC5AFFFDEC6B0FFDEC6B0FFDFC7B1FFDFC7
      B1FFF0F0F0FFF8F8F8FF888D8BFF0000000000000000000000018C8E8DFFFCFC
      FCFFFCFCFCFF858A88FFE2E3E5FF858A88FFE2E3E5FF858A88FFFCFCFCFFF8F8
      F8FFF6F6F6FFF5F5F5FFFDFDFDFF8C8E8DFF000000059D9185FFB1A396FF7D73
      69FF7A7066FF756B62FF6A6159FF2C2824FF544D46FF80746AFF7A7066FF756B
      62FF6E655CFF000000FE0000000500000000564F48F9403B36FF564F48FF3B36
      31FF312D29FF1B1916E5000000300E0D0CB5181614FFB5E6F5FF81D6EEFFB2E3
      F9FF8BC0E7FFAED3F6FFC4E0FCFF5E95C5F700000000858A88FFFCFCFCFFDAC2
      ACFFDAC2ACFFDBC3ADFFDCC4AEFFDDC5AFFFDDC5AFFFDEC6B0FFDEC6B0FFDEC6
      B0FFEEEEEEFFF8F8F8FF888D8BFF0000000000000000000000018C8E8DFFFDFD
      FDFFBCBCBCFF858A88FFE2E3E5FFE2E3E5FF858A88FFBCBCBCFFBCBCBCFFBCBC
      BCFFBCBCBCFFF1F1F1FFFDFDFDFF8C8E8DFF00000004887D73E1BAAEA2FF8275
      6BFF82756BFFAA9179FFBAA794FFAF9E88FAB09781FF9F8D7BFF836B59FF6F61
      55FF95897BFF020201E000000003000000009D9185FFB1A396FF7D7369FF7A70
      66FF756B62FF6A6159FF2C2824FF544D46FF80746AFF7A7066FFB1E6F5FF75BE
      E7FFB4D2F0FFE5F3FFFFACD2EFFF3A73A4E800000000858A88FFFCFCFCFFD9C1
      ABFFAF9781FFAF9781FFAF9781FFAF9781FFDCC4AEFFAF9781FFAF9781FFDDC5
      AFFFECECECFFF8F8F8FF888D8BFF0000000000000000000000018C8E8DFFFCFC
      FCFFFCFCFCFFFCFCFCFF858A88FF858A88FFF4F4F4FFF4F4F4FFF4F4F4FFF4F4
      F4FFF4F4F4FFF4F4F4FFFCFCFCFF8C8E8DFF000000080A0909489B8E82FF9D91
      85FF86796FFF544D46FF4E4842FF80746AFF6C645BFF826A56FFA6917BFF9484
      72FF544D46FF0202027A0000000100000000887D73E1BAAEA2FF82756BFF8275
      6BFFAA9179FFBAA794FFAF9E88FAB09781FF9F8D7BFF836B59FF6F6155FF94BD
      CCFF56A5D8FF85B1DBFF449DD0FF05141C5E00000000858A88FFFDFDFDFFD8C0
      AAFFAF9781FFAF9781FFAF9781FFAF9781FFDBC3ADFFDCC4AEFFDCC4AEFFDCC4
      AEFFEBEBEBFFF8F8F8FF888D8BFF0000000000000000000000018C8E8DFFFCFC
      FCFFACAEADFFACAEADFFACAEADFFACAEADFFACAEADFFACAEADFFACAEADFFACAE
      ADFFACAEADFFFCFCFCFFFDFDFDFF8C8E8DFF0000000000000000726960FFA497
      8AFF95897BFF9F9286FF3C3732FF000000004A443EFF7C7268FF85786EFF3C37
      32FF1D1A18A70000000C00000002000000000A0909489B8E82FF9D9185FF8679
      6FFF544D46FF4E4842FF80746AFF6C645BFF826A56FFA6917BFF948472FF544D
      46FF0202027A00000001000000000000000000000000858A88FFFDFDFDFFD8C0
      AAFFAF9781FFAF9781FFAF9781FFAF9781FFDBC3ADFFDBC3ADFFDBC3ADFFDBC3
      ADFFDBC3ADFFF8F8F8FF888D8BFF0000000000000000000000018C8E8DFFFDFD
      FDFFF9F9F9FFFAFAFAFFF9F9F9FFF9F9F9FFF6F6F6FFF4F4F4FFEFEFEFFFFDFD
      FDFFF8F8F8FFF7F7F7FF8C8E8DFFACAEADFF0000000000000000000000000000
      00006F665CE2C3B8AEFF635B53FF000000007A7066FFA89B8EFF7C7268E40000
      00000000000000000000000000000000000000000000726960FFA4978AFF9589
      7BFF9F9286FF3C3732FF000000004A443EFF7C7268FF85786EFF3C3732FF1D1A
      18A70000000C00000002000000000000000000000000858A88FFFEFEFEFFD7BF
      A9FFAF9781FFAF9781FFAF9781FFAF9781FFDAC2ACFFAF9781FFAF9781FFAF97
      81FFDAC2ACFFF8F8F8FF888D8BFF0000000000000000000000008C8E8DFFF8F8
      F8FDF6F6F6FFF7F7F7FFF8F8F8FFF7F7F7FFF5F5F5FFF3F3F3FFEEEEEEFFFCFC
      FCFFECECECFF8C8E8DFFACAEADFF000000020000000000000000000000000000
      00007A7066E2BCB0A4FF9D9185FF00000000AEA093FF9D9185FF49433DDA0000
      0000000000000000000000000000000000000000000000000000000000006F66
      5CE2C3B8AEFF635B53FF000000007A7066FFA89B8EFF7C7268E4000000000000
      00000000000000000000000000000000000000000000858A88FFFDFDFDFFD6BE
      A8FFD7BFA9FFD7BFA9FFD8C0AAFFD8C0AAFFD9C1ABFFD9C1ABFFD9C1ABFFD9C1
      ABFFD9C1ABFFF8F8F8FF888D8BFF0000000000000000000000008C8E8DFFDDDD
      DDF0F8F8F8FDFDFDFDFFFDFDFDFFFCFCFCFFFDFDFDFFFCFCFCFFFDFDFDFFFAFA
      FAFF8C8E8DFFACAEADFF00000002000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007A70
      66E2BCB0A4FF9D9185FF00000000AEA093FF9D9185FF49433DDA000000000000
      00000000000000000000000000000000000000000000838886FEFDFDFDFFFEFE
      FEFFFEFEFEFFFDFDFDFFFDFDFDFFFCFCFCFFFCFCFCFFFBFBFBFFFBFBFBFFFAFA
      FAFFFAFAFAFFF9F9F9FF868B89FE000000000000000000000000ACAEADFF8C8E
      8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E8DFF8C8E
      8DFFACAEADFF0000000200000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002D2F2E94858A88FF858A
      88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A88FF858A
      88FF858A88FF858A88FF2D2F2E94000000003A20108F5D341AB59F5A2DEEB766
      33FFB56633FFB46532FFB26432FFB06331FFAE6231FFAC6130FFAA6030FFA95F
      30FFA85E2FFFA55E2FFE94542BF163381CC4000000000000001D000000340000
      0036000000360000003600000036000000360000003600000036000000360000
      003600000036000000330000001D000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000F08035687481EFF87481EFF8748
      1EFF090502440000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008D4E27DEEBC6ADFFEAC5ADFFFEFB
      F8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFB
      F8FFFEFBF8FFC89A7AFFC79877FF905129ED0000000000000034E5E5E5F5F8F8
      F8FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFF8F8F8FDE1E1E1F3000000330000000000000000113E56FF13405CFF2367
      9CFF2A74B4FF1A3F55AD00000000000000000000000000000000000000000000
      00000000000000000000000000000000000087481EFFBEA592FFB89C86FFB89C
      86FF87481EFF0905024400000000000000000000000000000000000000002123
      22815A5D5CD3212322810000000000000000B86935FEEDCAB3FFE0A278FFFEFA
      F7FF60C088FF60C088FF60C088FF60C088FF60C088FF60C088FF60C088FF60C0
      88FFFDF9F6FFCA8D63FFC99B7AFFA55E2FFE0000000100000036FAFAFAFEFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFC
      FCFFFCFCFCFFF8F8F8FD000000360000000000000000104057FF5B9CD4FFA6CF
      F5FFA9CFECFF468BC1FF2A74B4FF000000000000000000000000000000000000
      00000000000000000000000000000000000087481EFFCCBBADFFA7917BFFB595
      78FFAE8B70FF87481EFF090502440000000000000000000000000B0C0B4C858A
      88FF9EA1A0FF858A88FF0000000000000000BB6A36FFEECCB6FFE1A278FFFEFA
      F7FFBFDCC2FFBFDCC2FFBFDCC2FFBFDCC2FFBFDCC2FFBFDCC2FFBFDCC2FFBFDC
      C2FFFDF9F6FFCD9066FFCC9E81FFA85F30FF0000000100000036FCFCFCFFFCFC
      FCFF164158FF295F89FF4A8ABEFF6DA8CBFFE0E9F1FFFBFBFBFFFBFBFBFFFBFB
      FBFFFBFBFBFFFCFCFCFF0000003600000001000000001C6B93FFCBE3F9FF5FAA
      ECFF3E98E8FF1365C2FF145EAAFF2A74B4FF0000000000000000000000000000
      00000000000000000000000000000000000087481EFFCCBBADFFA48D76FFA28A
      72FFB49578FFB3937AFF87481EFF00000000000000000B0C0B4C858A88FFD2D3
      D4FFC2C3C4FF858A88FF0000000000000000BB6936FFEFCEB8FFE1A277FFFEFA
      F7FF60C088FF60C088FF60C088FF60C088FF60C088FF60C088FF60C088FF60C0
      88FFFDF9F6FFCF9368FFCEA384FFAA5F30FF0000000100000036FCFCFCFFFCFC
      FCFF2C6585FF94C7F9FF91C9F9FF3F85C9FF2469AEFFD4E2EEFFFAFAFAFFFAFA
      FAFFFAFAFAFFFCFCFCFF0000003600000001000000001C6B93FFC8E1F2FFD1E7
      FAFF327BB5FF2F99C3FF6BC4DCFF489CCFFF3283C7FF00000000000000000000
      0000000000000000000000000000000000000402013087481EFFD4C8BDFFA48D
      76FFA48D76FFBEA592FF87481EFF000000000B0C0B4C858A88FFE2E3E4FFC2C4
      C6FF858A88FF212322810000000000000000BA6834FFEFD0BBFFE2A278FFFEFB
      F8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFBF8FFFEFB
      F8FFFEFBF8FFD3966BFFD2A78AFFAB6030FF0000000100000036FCFCFCFFFCFC
      FCFF4189AAFFE0F2FFFF529AD8FF1878BEFF4798C5FF468EC7FFD8E6F3FFF8F8
      F8FFF8F8F8FFFCFCFCFF000000360000000100000000000102202489B9FFB0CB
      E1FF65A9C8FF5EDCF5FF42D6F4FF8EEEFAFF5BB4E6FF398FD9FF000000000000
      000000000000000000000000000000000000000000000100001987481EFFCCBB
      ADFFCCBBADFFB3937AFF87481EFF21232281858A88FFD3D3D4FFBDBEBFFF858A
      88FF0B0C0B4C000000020000000000000000BB6834FFF0D2BEFFE2A378FFE2A3
      78FFE1A378FFE2A379FFE1A379FFE0A176FFDE9F75FFDD9F74FFDC9D72FFD99B
      70FFD8996FFFD6996EFFD5AB8EFFAD6131FF0000000100000036FCFCFCFFFCFC
      FCFFA5C3D7FF78B6D5FF90B7D1FF53C9E4FF59DFF5FF76D0EDFF4F9DDDFFDFEB
      F5FFF8F8F8FFFCFCFCFF00000036000000010000000000000000000000002489
      B9FFBEE6F2FFB3F4FCFF5EDCF5FF42D6F4FF8EEEFAFF5BB4E6FF398FD9FF0000
      0000000000000000000000000000000000000000000000000000010000198748
      1EFF87481EFF87481EFF85837BFFAAADADFFC8C9CAFFBDBEBFFF858A88FF0B0C
      0B4C00000000000000000000000000000000BB6834FFF2D5C2FFE3A378FFE3A3
      78FFE2A379FFE2A379FFE2A479FFE1A277FFE0A176FFDEA075FFDE9E73FFDC9D
      72FFDA9B71FFD99B71FFDAB095FFAF6231FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFB2D5E5FF74BAD7FFC2F6FDFF61DFF7FF5BE2F8FF77D3F0FF4798
      DCFFDEE9F2FFFCFCFCFF00000036000000010000000000000000000000000000
      00002590BFFFC3EDF8FFB3F4FCFF5EDCF5FF42D6F4FF8EEEFAFF5BB4E6FF398F
      D9FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000B5B7B8FF858A88FFB7B8B9FF858A88FF0B0C0B4C0000
      000200000000000000000000000000000000BB6834FFF2D8C5FFE3A479FFE3A3
      78FFE3A478FFE2A479FFE2A379FFE1A379FFE1A277FFDFA075FFDE9F74FFDD9E
      72FFDB9C70FFDC9D72FFDDB59AFFB16332FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFAFD4E5FF75CBE7FFC7F7FDFF5CDCF5FF58E1F7FF79D4
      F1FF499ADDFFD4E5F5FF00000036000000010000000000000000000000000000
      0000000000002DBAE4FFC3EDF8FFB3F4FCFF5EDCF5FF42D6F4FF8EEEFAFF5BB4
      E6FF398FD9FF000000000000000000000000000000000B0C0B4C858A88FF858A
      88FF858A88FF858A88FFD0D1D2FFA3A4A4FF858A88FF21232281000000000000
      000000000000000000000000000000000000BB6934FFF4D9C7FFE6A67BFFC88C
      62FFC98D63FFC98E65FFCB926AFFCB926BFFCA9067FFC88C63FFC88C62FFC88C
      62FFC88C62FFDA9C72FFE1BA9FFFB36432FF0000000100000036FCFCFCFFFCFC
      FCFFFCFCFCFFFCFCFCFFFCFCFCFFBDE5F2FF77D3EEFFC7F7FDFF5DDCF5FF59E2
      F7FF78D6F2FF4FA2E2FF04090D5C000000010000000000000000000000000000
      000000000000000000002DBAE4FFC3EDF8FFB3F4FCFF5EDCF5FF42D6F4FF8EEE
      FAFF5BB4E6FF398FD9FF00000000000000000B0C0B4C858A88FFF1F1F1FEEFF0
      F0FFEBECECFEE8EAEAFEB6B9BAFF858A88FF0B0C0B4C858A88FF0B0C0B4C0000
      000000000000000000000000000000000000B96934FEF4DCC9FFE7A77BFFF9EC
      E1FFF9ECE1FFF9EDE3FFFCF4EEFFFDFAF7FFFDF7F3FFFAEDE5FFF7E7DBFFF7E5
      D9FFF6E5D8FFDEA075FFE4BEA4FFB46532FF0000000100000036FCFCFCFFFBFB
      FBFFFCFCFCFFFCFCFCFFFBFBFBFFF8F8F8FFBAE3F0FF7BD4EEFFC4F6FDFF6ADD
      F6FF6BCAEDFF61A3D7FF5087B7F0010304270000000000000000000000000000
      00000000000000000000000000002DBAE4FFC3EDF8FFB3F4FCFF66D9F5FF6DCF
      F3FF579DD0FF71ABDDFF4D91C9FF00000000858A88FFF3F4F4FEA9ACABFF858A
      88FFF7F7F7FFE2E3E5FFAAADADFF3B3C3C81000000000B0C0B4C858A88FF0B0C
      0B4C00000000000000000000000000000000B46532FAF5DDCCFFE7A87CFFFAF0
      E8FFFAF0E8FFC98D64FFFAF0E9FFFDF8F3FFFEFAF8FFFCF4EFFFF9E9DFFFF7E7
      DBFFF7E5D9FFE0A276FFE7C2A9FFB66633FF0000000100000036FCFCFCFFF9F9
      F9FFF9F9F9FFF9F9F9FFF7F7F7FFF6F6F6FFF2F2F2FFA8D9E8FF81D6EEFFB2E3
      F9FF8BC0E7FFAED3F6FFC4E0FCFF5E95C5F70000000000000000000000000000
      0000000000000000000000000000000000002DBAE4FFC3EDF8FFA8E2F8FF6AAE
      DDFFA5CFF4FFA5CFF4FFBDDBF7FF4D89BEF70B0C0B4C858A88FF000000000B0C
      0B4C858A88FFFAFAFAFF858A88FF0000000000000000000000000B0C0B4C858A
      88FF878C8AFF0000004C0000000000000000A65D2EF0F6DFD0FFE8A87CFFFCF6
      F1FFFCF6F1FFC88C62FFFAF1E9FFFBF4EEFFFDFAF7FFFDF9F6FFFAF0E8FFF8E8
      DDFFF7E6DBFFE1A378FFEFD5C3FFB56733FE0000000100000036FCFCFCFFF7F7
      F7FFF9F9F9FFF7F7F7FFF7F7F7FFF3F3F3FFF0F0F0FFEAEAEAFFAFE4F3FF75BE
      E7FFB4D2F0FFE5F3FFFFACD2EFFF3A73A4E80000000000000000000000000000
      000000000000000000000000000000000000000000002DBAE4FFA7D4F4FFC5E1
      F8FFCCE3F9FFCCE3F9FFBDDBF7FF4D8EC5FD0000000000000000000000000000
      0000858A88FFEEF0F0FF858A88FF00000000000000000000000000000000858A
      88FFF0F0F0FF858A88FF0000004C00000000874B25D8F6DFD1FFE9AA80FFFEFA
      F6FFFDFAF6FFC88C62FFFBF3EEFFFBF1EAFFFCF6F2FFFEFBF8FFFCF6F1FFF9EC
      E2FFF8E7DBFFEED0BAFFECD0BDFFB1683AF80000000000000036F7F7F7FDF4F4
      F4FFF5F5F5FFF5F5F5FFF5F5F5FFF1F1F1FFEFEFEFFFE9E9E9FFFCFCFCFFACD5
      E4FF56A5D8FF85B1DBFF449DD0FF05141C5E0000000000000000000000000000
      00000000000000000000000000000000000000000000000000004EA8D9FF68A5
      D8FFC9E1F7FFCBE3F8FF4095CAFF163C5AAE000000000000000000000000858A
      88FFE9EBECFF858A88FF0B0C0B4C000000000000000000000000000000000000
      004C858A88FFE8E9E9FC858A88FF000000004526139BF6E0D1FFF7E0D1FFFEFB
      F8FFFEFBF7FFFDF9F6FFFCF5F0FFFAF0EAFFFBF2EDFFFDF9F6FFFDFAF7FFFBF1
      EBFFF6E7DDFEE4C8B7FBAC754FEC1B0F07630000000000000033DBDBDBF0F7F7
      F7FDFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFFCFCFCFFF8F8F8FF2F2F
      2F91000000200000000200000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000009418F
      B8EA4E91C6FD4C90C8FF2278A0DF000102190000000000000000000000000B0C
      0B4C858A88FF0B0C0B4C00000000000000000000000000000000000000000000
      00000000004C858A88FF0B0C0B4C0000000024140A713B211090784322CCA35C
      2EEEB46532FAB96934FEBB6934FFBB6834FFBB6834FFBC6A37FFBD6C39FFBB6B
      38FFA45C30EF764526CB130B055400000000000000000000001C000000330000
      0036000000360000003600000036000000360000003600000036000000360000
      0020000000020000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000A0A0E3F9F3139BCF711175BB90000
      011C000000000000000000000000000000000000000000000000000000000000
      00000000000000000000714024C1A05E36E6C67343FEB4693DF3B5693DF3B469
      3DF3B4693DF3B7693EF4A35F37E8B47049F10000000000000000000000000000
      00000000000000000002131313771919198A1818188A1717178A1616168A1515
      158A1515158A1414148A1313138A0A0A0A661C5991D12980CAF7297FCAF72980
      CAF72980CAF72980CAF72980CAF72980CAF72980CAF72980CAF72980CAF7297F
      CAF72980CAF71C5991D100000000000000000000000000000000000000000000
      021F00000326000000030000000003051D703E49D9FF1F268FDD3B45D1FF0B0F
      45A4000000000000000000000000000000000000000000000000000000000000
      00000000000000000000AC673EEDFCF3ECFFFAF1E8FFFAF0E7FFFBF1E9FFFBF2
      EAFFFBF2EAFFFBF2EBFFFDF4EEFFC0794EF902080C4A144063CF1E6198FF1E61
      98FF1E6198FF226195FF567188FFF7F7F7FFF0F0F0FFF0F0F0FFF0F0F0FFF0F0
      F0FFF0F0F0FFF0F0F0FFE9E9E9FA12121284318AD1FBDCF0FAFF98E1F6FF95E0
      F6FF92DFF6FF8EDEF5FF89DCF5FF85DAF4FF80D9F4FF78D7F3FF72D5F3FF6ED3
      F2FFC2EAF8FF3394DAFF00000000000000000000000000000000010316632E39
      BBF8353FCFFF0E1351B0000001180F1454AF333EC8FD00000118090C3995303A
      BAF8000000140000000000000000000000000000000000000000000000000000
      00000000000000000000CF8251FFEFF1E7FFFFE9D9FFFFEADBFFFFE9D9FFFFE7
      D7FFFFE5D2FFFFE2CBFFEFF2E8FFCE8154FF123857C260A5D7FF63A8DAFF62A6
      D9FF60A4D8FF609FD1FF738EA4FFEFEFEFFFE7E7E7FFE7E7E7FFE7E7E7FFE7E7
      E7FFE6E6E6FFE6E6E6FFE2E2E2FA13131384318ACCF7EFFAFEFF93E5F8FF8FE4
      F8FF89E3F8FF82E1F7FF78DFF7FF6FDEF6FF65DBF5FF59D8F4FF4BD4F3FF3ED1
      F2FFCAF2FBFF3394DAFF00000000000000000000000000000000222B8FDE2A32
      A1E5151B67C23D48DDFF111764C2161E70CB2C34B2F20000000303052177353F
      C8FF000002200000000000000000000000000000000000000000000000000000
      00000000000000000000C57F4EFBFBF5EEFFFFE9D9FFFFEADBFFFFE9D9FFFFE7
      D7FFFFE5D2FFFFE2CBFFFBF6EFFFCA8153FE1E6198FF66ABDCFF468ECFFF448B
      CEFF4187CDFF4284C6FF6685A1FFF0F0F0FFB4B4B4FFB4B4B4FFB4B4B4FFB4B4
      B4FFB4B4B4FFB3B3B3FFE3E3E3FA141414843292CEF8F2FAFDFF94E6F8FF92E5
      F8FF90E5F8FF8BE3F8FF86E2F7FF7DE1F7FF75DEF6FF6ADCF6FF5CD9F4FF4DD5
      F3FFCCF2FBFF3394DAFF000000000000000000000000000000002A32A6EC0F15
      50AF00000000080D3C99404ED2FFC9A173FE3F4DD6FF070C3D9D1F2684D42229
      91DF0000000A000000000000000000000000683E26B9824E30CE965732DD894F
      2FD48A4F2FD4894F2FD4CA8450FFFFF7F1FFFFE9D9FFFFEADBFFFFE9D9FFFFE7
      D7FFFFE5D2FFFFE2CBFFFFF7F1FFC98353FE1E6198FF67AEDCFF4893D1FF468F
      D0FF448BCEFF4588C7FF6A88A3FFF0F0F0FFE8E8E8FFE8E8E8FFE7E7E7FFE7E7
      E7FFE7E7E7FFE7E7E7FFE3E3E3FA141414843299D0F9F6FCFEFF94E5F8FF93E5
      F8FF93E5F8FF91E5F8FF93DBE9FF93D7E3FF93D2DCFF90CED7FF8CC8CFFF86C1
      C6FFC9D8D6FF3394DAFFA35F37E8B47049F100000000000000000A0E3E9C3944
      CCFF0000073A00010F533946D4FFDBBD9CFFEECCA6FF3E4ADEFF3841D1FF0204
      196900000000000000000000000000000000885434D3BEB7B2DEBDB6B0DEBDB5
      AFDEBDB6B0DEBDB6B0DEE4BA91FFFFF7F0FFFFE7D5FFFDE7D6FFFDE6D4FFFCE4
      D0FFFBE3CBFFFADCC2FFFEF3E8FFCA8454FE1E6198FF69B1DEFF4B97D3FF4993
      D2FF468FD0FF488CC9FF6D8BA5FFF1F1F1FFB6B6B6FFB5B5B5FFB5B5B5FFB4B4
      B4FFB4B4B4FFB4B4B4FFE3E3E3FA1515158433A0D0FAFEFFFFFFF8FDFFFFF6FD
      FFFFF5FCFFFFF3FCFEFF9AE4F4FF9AE6F7FF9BE6F6FF9DE5F5FF9EE5F5FF9FE5
      F4FFDAF3F8FF3394DAFFFDF4EEFFC0794EF900000000000000000000021F1E24
      83D73A44CFFF2D36B5F23D4AD7FFD8BC9AFFF6EAE1FF695131BF0705013B0000
      0001000000000000000000000000000000009C613DDEB5B6AFDEC1B0A4DEC1B0
      A5DEC1B0A4DEC1AFA3DEE4BB91FFFFF7F2FFFEE7D5FFFEE7D5FFFDE5D1FFFAE0
      CAFFF9DEC4FFF7D9BCFFFDF2E7FFCA8555FE1E6198FF6BB3DFFF4E9CD5FF4C98
      D3FF4994D1FF4A91CBFF6E8EA7FFF1F1F1FFE9E9E9FFE9E9E9FFE8E8E8FFE8E8
      E8FFE8E8E8FFE7E7E7FFE3E3E3FA1616168431A4D0FAE8F6FBFF6EBCE7FF53AA
      E2FF4BA5E0FF91C9EBFFFAF3EFFFFDFEFDFFFFFDFCFFFFFDFCFFFEFDFCFFFEFC
      FBFFFEFEFDFF3394DAFFEFF2E8FFCE8154FF0000000000000000000000000000
      021F090D3B991A2077CC04072C89776042CCF0E0D0FF6D5638C50000000B0000
      000000000000000000000000000000000000965F3BDBBDB9B4DEC1B0A4DEC1B0
      A5DEC1B0A4DEC1AFA3DEE4BB92FFFEF7F1FFFCE5D2FFFCE4D1FFFBE2CCFFF9DD
      C4FFF6D7BBFFF3D1AFFFFAEFE4FFCA8556FE1E6198FF6EB5E0FF509FD7FF4E9C
      D6FF4C98D4FF4D95CDFF7191AAFFF1F1F1FFB7B7B7FFB6B6B6FFB6B6B6FFB6B6
      B6FFB5B5B5FFB5B5B5FFE4E4E4FA171717843099C4F2F1FAFDFF94DEF5FF93DC
      F4FF62BCE9FF3394DAFF3394DAFF3394DAFF3394DAFF3394DAFF3394DAFF3394
      DAFF3394DAFF3394DAFFFBF6EFFFCA8153FE0000000000000000000000000000
      0000000000000000000200000000382D1C90F6EADDFFE1CDB4FF634E33BF0000
      00000000000000000000000000000000000094603BDBC1BBB6DEC1B0A4DEC1B0
      A5DEC1B0A4DEC1AFA3DEE4BB92FFFEF6F0FFFCE2CDFFFCE3CDFFFADFC8FFF7D9
      BCFFF5E9DDFFFAF3EBFFFBF8F3FFC88151FE1E6198FF71B7E1FF55A3D7FF51A0
      D7FF4E9DD5FF5099CFFF7394ACFFF8F8F8FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2
      F2FFF2F2F2FFF1F1F1FFEAEAEAFA181818842D9BC1F0F7FCFEFF8EE4F8FF91DE
      F5FF9FE0F5FFACE1F6FFCA8450FFFFF7F1FFFFE9D9FFFFEADBFFFFE9D9FFFFE7
      D7FFFFE5D2FFFFE2CBFFFFF7F1FFC98353FE0000000000000000000000000000
      000000000000000000000000000033281889F1E2D4FFBCA282F4F5EBE0FF3E2F
      1B9D0000000000000000000000000000000095613CDBC1BBB5DEC1AFA1DEBFAF
      A2DEBFAEA0DEBEAC9DDEE4BB93FFFEF5EDFFFCDEC5FFFBE0C7FFF9DCC2FFF5D3
      B4FFFEF9F3FFFAE2C4FFECC193FF402817931E6198FF74B9E2FF5AA7D9FF56A4
      D8FF51A0D7FF519ED5FF5F8BA9FF6288A1FF6287A1FF6186A0FF67879FFF4866
      81FF1E1E1E8A1E1E1E8A1D1D1D8A0E0E0E6332A9CEF8FDFEFEFFFEFFFFFFFEFE
      FFFFFDFEFFFFFEFFFFFFE4BA91FFFFF7F0FFFFE7D5FFFDE7D6FFFDE6D4FFFCE4
      D0FFFBE3CBFFFADCC2FFFEF3E8FFCA8454FE0000000000000000000000000000
      00000000000000000000000000002E241584EEDFCEFF170F04658E7759DAE5D4
      C1FD261D117B00000000000000000000000095623CDBC1BBB6DEC0AFA1DEC0AF
      A1DEBFAD9EDEBDA999DEE5BE96FFFFFFFEFFFDF3E9FFFDF3EAFFFCF2E8FFFAEF
      E3FFFAF2E7FFEABB88FF664129B30000000C1E6198FF78BBE3FF5FAADBFF58A5
      D9FF51A0D7FF509FD7FF509FD7FF509FD7FF509FD7FF509FD7FF60A3D8FF1E61
      98FF00000000000000000000000000000000227890D058BAD7FA5ABBD8FA5ABB
      D8FA5ABBD8FA59BBD8FAE4BB91FFFFF7F2FFFEE7D5FFFEE7D5FFFDE5D1FFFAE0
      CAFFF9DEC4FFF7D9BCFFFDF2E7FFCA8555FE0000000000000000000000000000
      0000000000000000000000000000291F127CE7D5C1FF0402002B0100001BA189
      6BE7A6927AE11A150E62000000000000000095623DDBC0BBB6DEBEAD9EDEBEAC
      9EDEBDAA9ADEBCA794DEEAC39DFFE6BF96FFE4BB92FFE4BB92FFC09462F5C193
      63F6956D44DA2F1D0F7E00000009000000001E6198FF7ABDE4FF63AEDDFF60AB
      DCFF5CA8DAFF5AA7D9FF5AA7D9FF5AA7D9FF5AA7D9FF509FD7FF60A3D8FF1E61
      98FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E4BB92FFFEF7F1FFFCE5D2FFFCE4D1FFFBE2CCFFF9DD
      C4FFF6D7BBFFF3D1AFFFFAEFE4FFCA8556FE0000000000000000000000000000
      0000000000000000000000000000241B0F75DEC9AFFF0201001F000000000302
      0026A58B67EB725A3DCB000000000000000095623DDBC0BAB5DEBEAA9BDEBEAB
      9BDEBDA997DEBBA48EDEB9B0A7DEBDB7B1DEBDBBB7DE986E49DC000000000000
      0000000000000000000000000000000000001E6198FF7DBFE4FF67B2DEFF489B
      DAFF4297DCFF4196DCFF4096DCFF4095DCFF3F95DBFF4F9ED6FF6AB2DEFF1E61
      98FF000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E4BB92FFFEF6F0FFFCE2CDFFFCE3CDFFFADFC8FFF7D9
      BCFFF5E9DDFFFAF3EBFFFBF8F3FFC88151FE0000000000000000000000000000
      000000000000000000000000000020180D6FC8B195F700000010000000000000
      00000402002C6B502ECB000000000000000094613EDAC0B9B3DEBEA895DEBDA9
      96DEBCA693DEB99F88DEC0BCB7DEBDAA94DEB2926FDE311F1280000000000000
      000000000000000000000000000000000000184B75E06FB1D9FE7CBFE4FF4C9D
      DFFFB5EEFDFF73D4F0FF73D4F0FFB5EEFDFF499BDEFF6CB4E0FF67ABD4F91C58
      8AF3000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E4BB93FFFEF5EDFFFCDEC5FFFBE0C7FFF9DCC2FFF5D3
      B4FFFEF9F3FFFAE2C4FFECC193FF402817930000000000000000000000000000
      00000000000000000000000000001C140B699D876AE000000009000000000000
      000000000000000000060000000000000000865737D0BCBCBCDBBFB7B0DEBFB7
      B0DEBEB6B0DEBDB5ABDEBDB6AFDEB08E66DE4C311F9C0000000A000000000000
      000000000000000000000000000000000000010305320F2F49B21E6198FF3573
      A4FFB6EFFEFF80DBF3FF80DBF3FFB6EFFEFF2C6CA1FF1E6198FF0C283FA5030A
      0F53000000000000000000000000000000000000000000000000000000000000
      00000000000000000000E5BE96FFFFFFFEFFFDF3E9FFFDF3EAFFFCF2E8FFFAEF
      E3FFFAF2E7FFEABB88FF664129B30000000C0000000000000000000000000000
      00000000000000000000000000000503013143311AA100000005000000000000
      00000000000000000000000000000000000058351FAA845636CE9A6641DE9564
      3EDB96643EDB95633FDB865634D024150C6E0000000800000000000000000000
      00000000000000000000000000000000000000000000000000000002042A1E61
      98FF1E6198FF1E6198FF1E6198FF1E6198FF1C5687F000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000EAC39DFFE6BF96FFE4BB92FFE4BB92FFC09462F5C193
      63F6956D44DA2F1D0F7E0000000900000000}
  end
end
