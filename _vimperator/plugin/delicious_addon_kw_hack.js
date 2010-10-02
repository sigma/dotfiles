bookmarks.getSearchURL = function(text, useDefsearch) {
  let url = null;
  let postData = {};
  let searchString =
      (useDefsearch ? option["defsearch"] + " " : "")
      + text;

  this.getSearchEngines();

  url = window.getShortcutOrURI(searchString, postData);

  if (url == searchString)
    return [null, null];

  if (postData && postData.value)
    return [url, postData.value];

  return [url, null]; // can be null
}
