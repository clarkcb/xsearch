/*******************************************************************************
ISearchOption

Search option interface

@author Cary Clark &lt;clarkcb@gmail.com&gt;
@version $Rev$
@copyright Cary Clark 2012
*******************************************************************************/

package javasearch;

public interface ISearchOption {

	public String getShortArg();
	
	public String getLongArg();
	
	public String getDescription();
	
	public String getSortArg();
}
