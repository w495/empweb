(function(){

if (!window.qx) window.qx = {};

qx.$$start = new Date();
  
if (!qx.$$environment) qx.$$environment = {};
var envinfo = {"qx.application":"zqr.Application","qx.revision":"","qx.theme":"zqr.theme.Theme","qx.version":"2.0.1"};
for (var k in envinfo) qx.$$environment[k] = envinfo[k];

if (!qx.$$libraries) qx.$$libraries = {};
var libinfo = {"__out__":{"sourceUri":"script"},"qx":{"resourceUri":"../lib/qooxdoo/framework/source/resource","sourceUri":"../lib/qooxdoo/framework/source/class","sourceViewUri":"https://github.com/qooxdoo/qooxdoo/blob/%{qxGitBranch}/framework/source/class/%{classFilePath}#L%{lineNumber}"},"zqr":{"resourceUri":"../source/resource","sourceUri":"../source/class"}};
for (var k in libinfo) qx.$$libraries[k] = libinfo[k];

qx.$$resources = {};
qx.$$translations = {"C":null,"ru":null};
qx.$$locales = {"C":null,"ru":null};
qx.$$packageData = {};

qx.$$loader = {
  parts : {"boot":[0]},
  packages : {"0":{"uris":["__out__:zqr.4340738ff822.js","qx:qx/Bootstrap.js","qx:qx/util/OOUtil.js","qx:qx/core/Environment.js","qx:qx/Mixin.js","qx:qx/core/Aspect.js","qx:qx/Interface.js","qx:qx/lang/Core.js","qx:qx/core/Property.js","qx:qx/Class.js","qx:qx/lang/Generics.js","qx:qx/data/MBinding.js","qx:qx/data/SingleValueBinding.js","qx:qx/lang/String.js","qx:qx/lang/Array.js","qx:qx/bom/client/Engine.js","qx:qx/lang/Type.js","qx:qx/core/Assert.js","qx:qx/type/BaseError.js","qx:qx/dev/StackTrace.js","qx:qx/bom/client/EcmaScript.js","qx:qx/lang/Function.js","qx:qx/core/ObjectRegistry.js","qx:qx/core/AssertionError.js","qx:qx/bom/client/Json.js","qx:qx/lang/JsonImpl.js","qx:qx/lang/Json.js","qx:qx/lang/Date.js","qx:qx/data/IListData.js","qx:qx/core/ValidationError.js","qx:qx/lang/RingBuffer.js","qx:qx/log/appender/RingBuffer.js","qx:qx/log/Logger.js","qx:qx/core/MLogging.js","qx:qx/dom/Node.js","qx:qx/bom/Event.js","qx:qx/event/Manager.js","qx:qx/event/GlobalError.js","qx:qx/core/WindowError.js","qx:qx/core/GlobalError.js","qx:qx/event/IEventHandler.js","qx:qx/event/Registration.js","qx:qx/core/MEvents.js","qx:qx/event/IEventDispatcher.js","qx:qx/core/MProperty.js","qx:qx/core/MAssert.js","qx:qx/core/Object.js","qx:qx/util/DisposeUtil.js","qx:qx/event/type/Event.js","qx:qx/util/ObjectPool.js","qx:qx/event/Pool.js","qx:qx/event/dispatch/Direct.js","qx:qx/event/handler/Object.js","qx:qx/event/type/Data.js","qx:qx/locale/MTranslation.js","qx:qx/application/IApplication.js","qx:qx/core/BaseInit.js","qx:qx/bom/client/OperatingSystem.js","qx:qx/event/type/Native.js","qx:qx/event/handler/Window.js","qx:qx/event/handler/Application.js","qx:qx/core/Init.js","qx:qx/application/AbstractGui.js","qx:qx/theme/manager/Meta.js","qx:qx/util/ValueManager.js","qx:qx/theme/manager/Color.js","qx:qx/util/ColorUtil.js","qx:qx/theme/manager/Decoration.js","qx:qx/ui/decoration/IDecorator.js","qx:qx/ui/decoration/Abstract.js","qx:qx/ui/decoration/DynamicDecorator.js","qx:qx/bom/element/Clip.js","qx:qx/bom/Style.js","qx:qx/bom/element/BoxSizing.js","qx:qx/bom/client/Css.js","qx:qx/bom/client/Browser.js","qx:qx/bom/element/Cursor.js","qx:qx/lang/Object.js","qx:qx/bom/element/Overflow.js","qx:qx/bom/element/Opacity.js","qx:qx/bom/element/Style.js","qx:qx/bom/Document.js","qx:qx/bom/Viewport.js","qx:qx/util/AliasManager.js","qx:qx/theme/manager/Font.js","qx:qx/bom/Font.js","qx:qx/bom/webfonts/WebFont.js","qx:qx/bom/webfonts/Manager.js","qx:qx/util/LibraryManager.js","qx:qx/bom/client/Transport.js","qx:qx/util/ResourceManager.js","qx:qx/event/Timer.js","qx:qx/dom/Element.js","qx:qx/bom/client/Html.js","qx:qx/bom/element/Attribute.js","qx:qx/bom/Stylesheet.js","qx:qx/bom/client/Stylesheet.js","qx:qx/util/Uri.js","qx:qx/bom/webfonts/Validator.js","qx:qx/bom/element/Dimension.js","qx:qx/theme/manager/Icon.js","qx:qx/theme/manager/Appearance.js","qx:qx/Theme.js","qx:qx/ui/tooltip/Manager.js","qx:qx/ui/core/MLayoutHandling.js","qx:qx/ui/core/LayoutItem.js","qx:qx/ui/core/queue/Layout.js","qx:qx/util/DeferredCallManager.js","qx:qx/util/DeferredCall.js","qx:qx/html/Element.js","qx:qx/event/handler/Appear.js","qx:qx/event/dispatch/AbstractBubbling.js","qx:qx/event/dispatch/DomBubbling.js","qx:qx/event/handler/Element.js","qx:qx/event/handler/UserAction.js","qx:qx/event/handler/Mouse.js","qx:qx/event/type/Dom.js","qx:qx/event/type/Mouse.js","qx:qx/event/type/MouseWheel.js","qx:qx/dom/Hierarchy.js","qx:qx/event/util/Keyboard.js","qx:qx/event/handler/Keyboard.js","qx:qx/event/type/KeyInput.js","qx:qx/event/type/KeySequence.js","qx:qx/event/handler/Focus.js","qx:qx/bom/Selection.js","qx:qx/bom/Range.js","qx:qx/util/StringSplit.js","qx:qx/event/handler/TouchCore.js","qx:qx/event/type/Touch.js","qx:qx/event/type/Tap.js","qx:qx/event/type/Swipe.js","qx:qx/bom/client/Event.js","qx:qx/event/handler/Orientation.js","qx:qx/event/type/Orientation.js","qx:qx/event/handler/Touch.js","qx:qx/event/handler/Input.js","qx:qx/event/handler/Capture.js","qx:qx/event/handler/DragDrop.js","qx:qx/event/type/Drag.js","qx:qx/event/handler/Offline.js","qx:qx/bom/Element.js","qx:qx/event/dispatch/MouseCapture.js","qx:qx/bom/Selector.js","qx:qx/bom/client/Plugin.js","qx:qx/xml/Document.js","qx:qx/bom/client/Xml.js","qx:qx/event/type/Focus.js","qx:qx/bom/element/Scroll.js","qx:qx/bom/element/Location.js","qx:qx/ui/core/queue/Manager.js","qx:qx/ui/core/queue/Widget.js","qx:qx/ui/core/queue/Visibility.js","qx:qx/ui/core/queue/Appearance.js","qx:qx/ui/core/queue/Dispose.js","qx:qx/ui/core/DecoratorFactory.js","qx:qx/html/Decorator.js","qx:qx/ui/core/Widget.js","qx:qx/ui/core/EventHandler.js","qx:qx/ui/layout/Abstract.js","qx:qx/type/BaseString.js","qx:qx/locale/LocalizedString.js","qx:qx/locale/Manager.js","qx:qx/bom/client/Locale.js","qx:qx/bom/element/Decoration.js","qx:qx/io/ImageLoader.js","qx:qx/bom/element/Background.js","qx:qx/html/Image.js","qx:qx/ui/core/MPlacement.js","qx:qx/event/Idle.js","qx:qx/util/placement/Placement.js","qx:qx/util/placement/AbstractAxis.js","qx:qx/util/placement/DirectAxis.js","qx:qx/util/placement/KeepAlignAxis.js","qx:qx/util/placement/BestFitAxis.js","qx:qx/ui/basic/Image.js","qx:qx/ui/core/DragDropCursor.js","qx:qx/ui/core/MChildrenHandling.js","qx:qx/ui/container/Composite.js","qx:qx/ui/popup/Popup.js","qx:qx/ui/popup/Manager.js","qx:qx/ui/tooltip/ToolTip.js","qx:qx/ui/layout/Grow.js","qx:qx/ui/basic/Atom.js","qx:qx/ui/layout/Atom.js","qx:qx/ui/layout/Util.js","qx:qx/ui/form/IStringForm.js","qx:qx/ui/basic/Label.js","qx:qx/html/Label.js","qx:qx/bom/Label.js","qx:qx/ui/form/IForm.js","qx:qx/application/Inline.js","qx:qx/ui/window/MDesktop.js","qx:qx/ui/core/MBlocker.js","qx:qx/ui/core/Blocker.js","qx:qx/ui/root/Abstract.js","qx:qx/ui/core/FocusHandler.js","qx:qx/ui/root/Application.js","qx:qx/ui/layout/Canvas.js","qx:qx/html/Root.js","qx:qx/html/Blocker.js","qx:qx/ui/root/Page.js","qx:qx/ui/layout/Basic.js","zqr:zqr/Application.js","zqr:zqr/util/utils.js","qx:qx/io/remote/Request.js","qx:qx/io/remote/RequestQueue.js","qx:qx/io/remote/Exchange.js","qx:qx/io/remote/transport/Abstract.js","qx:qx/io/remote/transport/Iframe.js","qx:qx/event/handler/Iframe.js","qx:qx/bom/Iframe.js","qx:qx/io/remote/transport/Script.js","qx:qx/io/remote/transport/XmlHttp.js","qx:qx/io/remote/Response.js","qx:qx/ui/core/MExecutable.js","qx:qx/util/PropertyUtil.js","qx:qx/ui/form/IExecutable.js","qx:qx/ui/form/Button.js","qx:qx/ui/core/MRemoteChildrenHandling.js","qx:qx/ui/core/MRemoteLayoutHandling.js","qx:qx/ui/window/IWindowManager.js","qx:qx/ui/window/IDesktop.js","qx:qx/ui/window/Manager.js","qx:qx/ui/core/MMovable.js","qx:qx/ui/core/MContentPadding.js","qx:qx/ui/core/MResizable.js","qx:qx/ui/window/Window.js","qx:qx/ui/layout/VBox.js","qx:qx/ui/layout/HBox.js","qx:qx/ui/layout/Grid.js","zqr:zqr/Config.js","qx:qx/ui/layout/Dock.js","zqr:zqr/view/NavMixin.js","zqr:zqr/util/errors.js","zqr:zqr/view/NavBar.js","qx:qx/ui/toolbar/ToolBar.js","qx:qx/ui/core/Spacer.js","qx:qx/ui/toolbar/Separator.js","qx:qx/ui/form/MenuButton.js","qx:qx/ui/menu/Manager.js","qx:qx/ui/menu/Menu.js","qx:qx/ui/menu/Layout.js","qx:qx/ui/menu/Separator.js","qx:qx/ui/menu/AbstractButton.js","qx:qx/ui/menu/ButtonLayout.js","qx:qx/ui/container/SlideBar.js","qx:qx/ui/form/RepeatButton.js","qx:qx/event/AcceleratingTimer.js","qx:qx/ui/core/scroll/ScrollPane.js","qx:qx/ui/menu/MenuSlideBar.js","qx:qx/ui/form/HoverButton.js","qx:qx/ui/menubar/Button.js","qx:qx/ui/menu/Button.js","qx:qx/ui/toolbar/Part.js","qx:qx/ui/toolbar/PartContainer.js","qx:qx/ui/toolbar/MenuButton.js","qx:qx/ui/toolbar/Button.js","qx:qx/ui/window/Desktop.js","qx:qx/ui/root/Inline.js","qx:qx/event/handler/ElementResize.js","zqr:zqr/view/Controller/AbstructTabController.js","zqr:zqr/view/Controller/TabController.js","qx:qx/ui/form/MForm.js","qx:qx/ui/groupbox/GroupBox.js","zqr:zqr/view/ToolBar.js","zqr:zqr/view/Form/AbstractForm.js","qx:qx/ui/form/Form.js","qx:qx/ui/form/validation/Manager.js","qx:qx/ui/form/validation/AsyncValidator.js","qx:qx/ui/core/ISingleSelection.js","qx:qx/ui/form/Resetter.js","qx:qx/data/controller/ISelection.js","qx:qx/ui/form/IBooleanForm.js","qx:qx/ui/form/IColorForm.js","qx:qx/ui/form/IDateForm.js","qx:qx/ui/form/INumberForm.js","zqr:zqr/view/Form/GenericForm.js","qx:qx/ui/form/AbstractField.js","qx:qx/html/Input.js","qx:qx/bom/Input.js","qx:qx/ui/form/TextField.js","qx:qx/ui/form/TextArea.js","qx:qx/type/BaseArray.js","qx:qx/bom/element/Class.js","qx:qx/bom/Collection.js","qx:qx/bom/Html.js","qx:qx/bom/request/Script.js","qx:qx/event/Emitter.js","qx:qx/ui/form/IRadioItem.js","qx:qx/ui/core/MSingleSelectionHandling.js","qx:qx/ui/core/SingleSelectionManager.js","qx:qx/ui/core/ISingleSelectionProvider.js","qx:qx/ui/form/MModelSelection.js","qx:qx/data/marshal/MEventBubbling.js","qx:qx/data/Array.js","qx:qx/ui/form/IModelSelection.js","qx:qx/ui/form/RadioGroup.js","qx:qx/ui/form/ToggleButton.js","qx:qx/ui/form/MModelProperty.js","qx:qx/ui/form/IModel.js","qx:qx/ui/form/CheckBox.js","qx:qx/ui/form/DateField.js","qx:qx/locale/Date.js","qx:qx/util/format/IFormat.js","qx:qx/util/format/DateFormat.js","qx:qx/ui/control/DateChooser.js","qx:qx/ui/form/PasswordField.js","qx:qx/ui/form/IRange.js","qx:qx/ui/form/Spinner.js","qx:qx/locale/Number.js","qx:qx/ui/form/AbstractSelectBox.js","qx:qx/ui/core/selection/Abstract.js","qx:qx/ui/core/selection/Widget.js","qx:qx/ui/core/selection/ScrollArea.js","qx:qx/ui/core/MMultiSelectionHandling.js","qx:qx/ui/core/IMultiSelection.js","qx:qx/ui/core/scroll/MWheelHandling.js","qx:qx/ui/core/scroll/MScrollBarFactory.js","qx:qx/ui/core/scroll/IScrollBar.js","qx:qx/ui/core/scroll/NativeScrollBar.js","qx:qx/ui/core/scroll/ScrollBar.js","qx:qx/ui/form/Slider.js","qx:qx/ui/core/scroll/ScrollSlider.js","qx:qx/ui/core/scroll/AbstractScrollArea.js","qx:qx/bom/client/Scroll.js","qx:qx/ui/form/List.js","qx:qx/bom/String.js","qx:qx/util/StringEscape.js","qx:qx/ui/form/SelectBox.js","qx:qx/ui/form/ListItem.js","qx:qx/util/Validate.js","qx:qx/ui/form/renderer/IFormRenderer.js","qx:qx/ui/form/renderer/AbstractRenderer.js","qx:qx/ui/form/renderer/Single.js","zqr:zqr/view/HorisontalRenderer.js","zqr:zqr/view/MegaRenderer.js","qx:qx/ui/form/renderer/Double.js","qx:qx/data/controller/Form.js","qx:qx/data/marshal/IMarshaler.js","qx:qx/data/marshal/Json.js","qx:qx/data/controller/Object.js","qx:qx/util/Serializer.js","qx:qx/ui/table/Table.js","qx:qx/ui/table/IRowRenderer.js","qx:qx/ui/table/rowrenderer/Default.js","qx:qx/ui/table/IColumnMenuButton.js","qx:qx/ui/table/columnmenu/Button.js","qx:qx/ui/table/IColumnMenuItem.js","qx:qx/ui/menu/CheckBox.js","qx:qx/ui/table/columnmenu/MenuItem.js","qx:qx/ui/table/selection/Manager.js","qx:qx/ui/table/selection/Model.js","qx:qx/ui/table/ICellRenderer.js","qx:qx/ui/table/cellrenderer/Abstract.js","qx:qx/ui/table/cellrenderer/Default.js","qx:qx/util/format/NumberFormat.js","qx:qx/ui/table/ICellEditorFactory.js","qx:qx/ui/table/celleditor/AbstractField.js","qx:qx/ui/table/celleditor/TextField.js","qx:qx/ui/table/IHeaderRenderer.js","qx:qx/ui/table/headerrenderer/Default.js","qx:qx/ui/table/headerrenderer/HeaderCell.js","qx:qx/ui/table/columnmodel/Basic.js","qx:qx/ui/table/pane/Pane.js","qx:qx/ui/table/pane/Header.js","qx:qx/ui/core/scroll/MTouchScroll.js","qx:qx/ui/table/pane/Scroller.js","qx:qx/ui/table/pane/Clipper.js","qx:qx/ui/table/pane/FocusIndicator.js","qx:qx/ui/table/pane/CellEvent.js","qx:qx/lang/Number.js","qx:qx/ui/table/pane/Model.js","qx:qx/ui/table/ITableModel.js","qx:qx/ui/table/model/Abstract.js","qx:qx/ui/table/model/Simple.js","zqr:zqr/view/GenericTable.js","qx:qx/ui/table/columnmodel/Resize.js","qx:qx/ui/core/ColumnData.js","qx:qx/ui/table/columnmodel/resizebehavior/Abstract.js","qx:qx/ui/table/columnmodel/resizebehavior/Default.js","zqr:zqr/view/GenericTableModel.js","qx:qx/ui/table/cellrenderer/AbstractImage.js","qx:qx/ui/table/cellrenderer/Boolean.js","qx:qx/ui/table/celleditor/CheckBox.js","qx:qx/ui/table/cellrenderer/Conditional.js","qx:qx/ui/table/cellrenderer/Date.js","qx:qx/ui/table/cellrenderer/Number.js","zqr:zqr/view/Pager.js","zqr:zqr/view/Controller/DirDoubleTabController.js","qx:qx/ui/splitpane/Pane.js","qx:qx/ui/splitpane/Slider.js","qx:qx/ui/splitpane/Splitter.js","qx:qx/ui/splitpane/Blocker.js","qx:qx/ui/splitpane/VLayout.js","qx:qx/ui/splitpane/HLayout.js","zqr:zqr/view/Controller/FormController.js","zqr:zqr/view/StaticForms.js","zqr:zqr/view/Form/AcvVideoCreateMaster.js","zqr:zqr/view/Form/AcvVideoCreateMaster/BasePage.js","zqr:zqr/view/Form/AcvVideoCreateMaster/Common.js","zqr:zqr/view/Form/AcvVideoCreateMaster/Upload.js","zqr:zqr/view/Form/Upload/UploadButton.js","zqr:zqr/view/Form/Upload/UploadForm.js","zqr:zqr/view/Form/Upload/UploadFakeStatusBar.js","zqr:zqr/view/Form/AcvVideoCreateMaster/Show.js","qx:qx/ui/embed/Flash.js","qx:qx/html/Flash.js","qx:qx/bom/Flash.js","qx:qx/util/TimerManager.js","qx:qx/ui/groupbox/CheckGroupBox.js","zqr:zqr/view/Form/AcvVideoCreateMaster/Events.js","zqr:zqr/view/Addline.js","qx:qx/ui/container/Scroll.js","zqr:zqr/view/Form/AcvVideoCreateMaster/UsersTargeting.js","zqr:zqr/view/Form/AcvVideoCreateMaster/RegionTargeting.js","qx:qx/ui/tree/selection/SelectionManager.js","qx:qx/ui/tree/core/AbstractItem.js","qx:qx/ui/tree/core/FolderOpenButton.js","qx:qx/ui/tree/core/AbstractTreeItem.js","qx:qx/ui/tree/Tree.js","zqr:zqr/view/SelListTreeDirs.js","qx:qx/ui/tree/TreeFolder.js","zqr:zqr/view/Form/AcvVideoCreateMaster/SubnetTargeting.js","zqr:zqr/view/Form/AcvVideoCreateMaster/PlatfromTargeting.js","zqr:zqr/view/Sltdac.js","zqr:zqr/view/SortedSelListTree.js","qx:qx/ui/tree/TreeFile.js","zqr:zqr/view/Form/AcvVideoCreateMaster/PageurlTargeting.js","zqr:zqr/view/Form/AcvVideoCreateMaster/CategoryTargeting.js","zqr:zqr/view/Form/BaseForm.js","zqr:zqr/view/Form/AcvVideoShow.js","qx:qx/ui/tabview/Page.js","qx:qx/ui/form/RadioButton.js","qx:qx/ui/tabview/TabButton.js","qx:qx/ui/core/MNativeOverflow.js","qx:qx/ui/embed/Html.js","zqr:zqr/view/SelListTree.js","qx:qx/ui/tabview/TabView.js","qx:qx/ui/container/Stack.js","zqr:zqr/view/Form/CustomerGroupForm.js","zqr:zqr/view/Form/CustomerUpdateForm.js","zqr:zqr/view/Form/CustomerViewForm.js","zqr:zqr/view/Form/CustomerProfileForm.js","zqr:zqr/view/Form/BillMakeForm.js","zqr:zqr/view/Form/BillViewForm.js","zqr:zqr/view/Form/BillPayForm.js","zqr:zqr/view/Form/SysvarForm.js","zqr:zqr/view/Form/SnetForm.js","zqr:zqr/view/Form/PlatformForm.js","zqr:zqr/view/Form/PageurlForm.js","zqr:zqr/view/Form/SnetRegionForm.js","zqr:zqr/view/Form/ConfigForm.js","qx:qx/theme/modern/Color.js","zqr:zqr/theme/Color.js","qx:qx/ui/decoration/MBackgroundColor.js","qx:qx/ui/decoration/MBackgroundImage.js","qx:qx/ui/decoration/MSingleBorder.js","qx:qx/ui/decoration/Single.js","qx:qx/ui/decoration/MBorderRadius.js","qx:qx/ui/decoration/Background.js","qx:qx/ui/decoration/Beveled.js","qx:qx/ui/decoration/MLinearBackgroundGradient.js","qx:qx/ui/decoration/Grid.js","qx:qx/ui/decoration/css3/BorderImage.js","qx:qx/ui/decoration/GridDiv.js","qx:qx/ui/decoration/Uniform.js","qx:qx/ui/decoration/MDoubleBorder.js","qx:qx/ui/decoration/MBoxShadow.js","qx:qx/theme/modern/Decoration.js","zqr:zqr/theme/Decoration.js","qx:qx/theme/modern/Appearance.js","zqr:zqr/theme/Appearance.js","qx:qx/theme/icon/Tango.js","qx:qx/theme/modern/Font.js","zqr:zqr/theme/Font.js","zqr:zqr/theme/Theme.js"]}},
  urisBefore : [],
  cssBefore : [],
  boot : "boot",
  closureParts : {},
  bootIsInline : false,
  addNoCacheParam : false,
  
  decodeUris : function(compressedUris)
  {
    var libs = qx.$$libraries;
    var uris = [];
    for (var i=0; i<compressedUris.length; i++)
    {
      var uri = compressedUris[i].split(":");
      var euri;
      if (uri.length==2 && uri[0] in libs) {
        var prefix = libs[uri[0]].sourceUri;
        euri = prefix + "/" + uri[1];
      } else {
        euri = compressedUris[i];
      }
      if (qx.$$loader.addNoCacheParam) {
        euri += "?nocache=" + Math.random();
      }
      
      uris.push(euri);
    }
    return uris;      
  }
};  

function loadScript(uri, callback) {
  var elem = document.createElement("script");
  elem.charset = "utf-8";
  elem.src = uri;
  elem.onreadystatechange = elem.onload = function() {
    if (!this.readyState || this.readyState == "loaded" || this.readyState == "complete") {
      elem.onreadystatechange = elem.onload = null;
      if (typeof callback === "function") {
        callback();
      }
    }
  };

  if (isLoadParallel) {
    elem.async = null;
  }

  var head = document.getElementsByTagName("head")[0];
  head.appendChild(elem);
}

function loadCss(uri) {
  var elem = document.createElement("link");
  elem.rel = "stylesheet";
  elem.type= "text/css";
  elem.href= uri;
  var head = document.getElementsByTagName("head")[0];
  head.appendChild(elem);
}

var isWebkit = /AppleWebKit\/([^ ]+)/.test(navigator.userAgent);
var isLoadParallel = 'async' in document.createElement('script');

function loadScriptList(list, callback) {
  if (list.length == 0) {
    callback();
    return;
  }

  var item;

  if (isLoadParallel) {
    while (list.length) {
      item = list.shift();
      if (list.length) {
        loadScript(item);
      } else {
        loadScript(item, callback);
      }
    }
  } else {
    item = list.shift();
    loadScript(item,  function() {
      if (isWebkit) {
        // force async, else Safari fails with a "maximum recursion depth exceeded"
        window.setTimeout(function() {
          loadScriptList(list, callback);
        }, 0);
      } else {
        loadScriptList(list, callback);
      }
    });
  }
}

var fireContentLoadedEvent = function() {
  qx.$$domReady = true;
  document.removeEventListener('DOMContentLoaded', fireContentLoadedEvent, false);
};
if (document.addEventListener) {
  document.addEventListener('DOMContentLoaded', fireContentLoadedEvent, false);
}

qx.$$loader.importPackageData = function (dataMap, callback) {
  if (dataMap["resources"]){
    var resMap = dataMap["resources"];
    for (var k in resMap) qx.$$resources[k] = resMap[k];
  }
  if (dataMap["locales"]){
    var locMap = dataMap["locales"];
    var qxlocs = qx.$$locales;
    for (var lang in locMap){
      if (!qxlocs[lang]) qxlocs[lang] = locMap[lang];
      else 
        for (var k in locMap[lang]) qxlocs[lang][k] = locMap[lang][k];
    }
  }
  if (dataMap["translations"]){
    var trMap   = dataMap["translations"];
    var qxtrans = qx.$$translations;
    for (var lang in trMap){
      if (!qxtrans[lang]) qxtrans[lang] = trMap[lang];
      else 
        for (var k in trMap[lang]) qxtrans[lang][k] = trMap[lang][k];
    }
  }
  if (callback){
    callback(dataMap);
  }
}

qx.$$loader.signalStartup = function () 
{
  qx.$$loader.scriptLoaded = true;
  if (window.qx && qx.event && qx.event.handler && qx.event.handler.Application) {
    qx.event.handler.Application.onScriptLoaded();
    qx.$$loader.applicationHandlerReady = true; 
  } else {
    qx.$$loader.applicationHandlerReady = false;
  }
}

// Load all stuff
qx.$$loader.init = function(){
  var l=qx.$$loader;
  if (l.cssBefore.length>0) {
    for (var i=0, m=l.cssBefore.length; i<m; i++) {
      loadCss(l.cssBefore[i]);
    }
  }
  if (l.urisBefore.length>0){
    loadScriptList(l.urisBefore, function(){
      l.initUris();
    });
  } else {
    l.initUris();
  }
}

// Load qooxdoo boot stuff
qx.$$loader.initUris = function(){
  var l=qx.$$loader;
  var bootPackageHash=l.parts[l.boot][0];
  if (l.bootIsInline){
    l.importPackageData(qx.$$packageData[bootPackageHash]);
    l.signalStartup();
  } else {
    loadScriptList(l.decodeUris(l.packages[l.parts[l.boot][0]].uris), function(){
      // Opera needs this extra time to parse the scripts
      window.setTimeout(function(){
        l.importPackageData(qx.$$packageData[bootPackageHash] || {});
        l.signalStartup();
      }, 0);
    });
  }
}
})();



qx.$$loader.init();

