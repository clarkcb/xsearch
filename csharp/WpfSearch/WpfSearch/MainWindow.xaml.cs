using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using System.Windows;
using System.Windows.Data;
using System.Windows.Forms;
using CsSearch;

namespace WpfSearch {
	/// <summary>`
	/// Interaction logic for MainWindow.xaml
	/// </summary>
	public partial class MainWindow : Window
	{
		private FolderBrowserDialog folderBrowserDialog;

		public MainWindow()
		{
			folderBrowserDialog = new FolderBrowserDialog();
			InitializeComponent();
			// temporary for testing
			SetTestValues();
		}

		private void SetTestValues()
		{
			tbStartDir.Text = @"C:\Work\PurchasEdge\Phoenix.Dev";
			tbIncludeExtensions.Text = "cs";
			tbIncludeFilePattern.Text = "ViewModelProvider";
			tbSearchPattern.Text = "Enroll";
		}

		private void btnBrowse_Click(object sender, RoutedEventArgs e)
		{
			var result = folderBrowserDialog.ShowDialog();
			if (result == System.Windows.Forms.DialogResult.OK) {
				tbStartDir.Text = folderBrowserDialog.SelectedPath;
			}
		}

		private void btnSearch_Click(object sender, RoutedEventArgs e)
		{
			Search();
		}

		private void Search()
		{
			SearchSettings settings;
			try
			{
				settings = GetSearchSettings();
			}
			catch (InvalidDataException e)
			{
				//TODO: show error dialog
				return;
			}
			var searcher = new Searcher(settings);
			try {
				searcher.Search();
			}
			catch (Exception)
			{
				return;
			}
			ShowResults(searcher.Results);
		}

		private void ShowResults(IEnumerable<SearchResult> results)
		{
			//dgSearchResults.MinRowHeight = 20;
			dgSearchResults.ItemsSource = new SearchResults(results);
		}

		private SearchSettings GetSearchSettings()
		{
			var settings = new SearchSettings();
			if (string.IsNullOrWhiteSpace(tbStartDir.Text))
			{
				throw new InvalidDataException("Missing starting directory");
			}
			settings.StartPath = tbStartDir.Text;

			if (string.IsNullOrWhiteSpace(tbSearchPattern.Text))
			{
				throw new InvalidDataException("Missing search pattern");
			}
			settings.AddSearchPattern(tbSearchPattern.Text);

			if (!string.IsNullOrWhiteSpace(tbIncludeExtensions.Text))
			{
				foreach (var x in Regex.Split(tbIncludeExtensions.Text, @"[,;\s]+"))
				{
					settings.AddInExtension(x);
				}
			}
			if (!string.IsNullOrWhiteSpace(tbExcludeExtensions.Text))
			{
				foreach (var x in Regex.Split(tbExcludeExtensions.Text, @"[,;\s]+"))
				{
					settings.AddOutExtension(x);
				}
			}
			if (!string.IsNullOrWhiteSpace(tbIncludeFilePattern.Text))
			{
				settings.AddInFilePattern(tbIncludeFilePattern.Text);
			}
			if (!string.IsNullOrWhiteSpace(tbExcludeFilePattern.Text))
			{
				settings.AddOutFilePattern(tbExcludeFilePattern.Text);
			}
			return settings;
		}
	}

	public class SearchResults : ObservableCollection<SearchResult>
	{
		public SearchResults(IEnumerable<SearchResult> searchResults)
		{
			foreach (var s in searchResults.OrderBy(s => s.File.FullName))
			{
				var trimResult = new SearchResult(s.SearchPattern, s.File, s.LineNum, s.Line.Trim());
				Add(trimResult);
			}
		}
	}
}
