<?php declare(strict_types=1);

use phpfind\FileResult;
use phpfind\FileType;
use phpsearch\SearchResult;
use phpsearch\SearchResultFormatter;
use phpsearch\SearchSettings;
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

class SearchResultTest extends TestCase
{
    const CSSEARCHPATH = '~/src/xsearch/csharp/CsSearch/CsSearch';
    public function test_single_line_search_result()
    {
        $settings = new SearchSettings();
        $settings->colorize = false;
        $formatter = new SearchResultFormatter($settings);
        $pattern = "Search";
        $file = new FileResult(self::CSSEARCHPATH, "Searcher.cs", FileType::Code, false);
        $line_num = 10;
        $match_start_index = 15;
        $match_end_index = 23;
        $line = "\tpublic class Searcher\n";
        $lines_before = [];
        $lines_after = [];
        $search_result = new SearchResult(
            $pattern,
            $file,
            $line_num,
            $match_start_index,
            $match_end_index,
            $line,
            $lines_before,
            $lines_after
        );
        $expected_output = sprintf(
            "%s: %d: [%d:%d]: %s",
            $file,
            $line_num,
            $match_start_index,
            $match_end_index,
            trim($line)
        );
        $output = $formatter->format($search_result);
        $this->assertEquals($expected_output, $output);
    }

    public function test_single_line_longer_than_maxlength_search_result()
    {
        $settings = new SearchSettings();
        $settings->colorize = false;
        $settings->max_line_length = 100;
        $formatter = new SearchResultFormatter($settings);
        $pattern = "maxlen";
        $file = new FileResult('.', "maxlen.txt", FileType::Text, false);
        $line_num = 1;
        $match_start_index = 53;
        $match_end_index = 59;
        $line = "0123456789012345678901234567890123456789012345678901maxlen8901234567890123456789012345678901234567890123456789";
        $lines_before = [];
        $lines_after = [];
        $search_result = new SearchResult(
            $pattern,
            $file,
            $line_num,
            $match_start_index,
            $match_end_index,
            $line,
            $lines_before,
            $lines_after
        );
        $expectedline = '...89012345678901234567890123456789012345678901maxlen89012345678901234567890123456789012345678901...';
        $expected_output = sprintf(
            "%s: %d: [%d:%d]: %s",
            $file,
            $line_num,
            $match_start_index,
            $match_end_index,
            $expectedline
        );
        $output = $formatter->format($search_result);
        $this->assertEquals($expected_output, $output);
    }

    public function test_binary_file_search_result()
    {
        $settings = new SearchSettings();
        $formatter = new SearchResultFormatter($settings);
        $pattern = "Search";
        $file = new FileResult(self::CSSEARCHPATH, "Searcher.exe", FileType::Binary, false);
        $line_num = 0;
        $match_start_index = 0;
        $match_end_index = 0;
        $line = null;
        $lines_before = [];
        $lines_after = [];
        $search_result = new SearchResult(
            $pattern,
            $file,
            $line_num,
            $match_start_index,
            $match_end_index,
            $line,
            $lines_before,
            $lines_after
        );
        $expected_output = sprintf("%s matches at [%d:%d]", $file, $match_start_index, $match_end_index);
        $output = $formatter->format($search_result);
        $this->assertEquals($expected_output, $output);
    }

    public function test_multi_line_search_result()
    {
        $settings = new SearchSettings();
        $settings->colorize = false;
        $formatter = new SearchResultFormatter($settings);
        $pattern = "Search";
        $file = new FileResult(self::CSSEARCHPATH, "Searcher.cs", FileType::Code, false);
        $line_num = 10;
        $match_start_index = 15;
        $match_end_index = 23;
        $line = "\tpublic class Searcher\n";
        $lines_before = ["namespace CsSearch\n", "{\n"];
        $lines_after = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];
        $search_result = new SearchResult(
            $pattern,
            $file,
            $line_num,
            $match_start_index,
            $match_end_index,
            $line,
            $lines_before,
            $lines_after
        );
        $output_template =
            "================================================================================\n" .
            "%s: %d: [%d:%d]\n" .
            "--------------------------------------------------------------------------------\n" .
            "   8 | namespace CsSearch\n" .
            "   9 | {\n" .
            "> 10 | \tpublic class Searcher\n" .
            "  11 | \t{\n" .
            "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
        $expected_output = sprintf(
            $output_template,
            $file,
            $line_num,
            $match_start_index,
            $match_end_index
        );
        $output = $formatter->format($search_result);
        $this->assertEquals($expected_output, $output);
    }
}
